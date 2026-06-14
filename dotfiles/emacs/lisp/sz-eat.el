;; === Eat terminal
;;
;; Eat has its own input modes (`semi-char', `char', `emacs', `line').
;; Meow has independent editing states.  This file keeps them in sync:
;; terminal input modes map to Meow `term', while Eat's read-only
;; `emacs-mode' maps to our Meow state for scrollback navigation and
;; mouse selection.

(defun sz/eat-setup-buffer ()
  "Apply local UI rules for Eat buffers."
  (setq-local cursor-in-non-selected-windows nil)
  ;; Avoid stale/small PTY sizes when an Eat buffer is shown in more than one
  ;; window.  A too-small terminal height makes pagers render only part of the
  ;; selected window, leaving blank space below.
  (setq-local window-adjust-process-window-size-function
              #'window-adjust-process-window-size-largest)
  ;; If point is moved to the live terminal cursor from off-screen, keep Emacs
  ;; from recentering it by default.
  (setq-local scroll-conservatively 101))

(defun sz/eat-ignore-mouse-click (_event)
  "Ignore mouse clicks in Eat buffers."
  (interactive "e"))

(defvar sz/eat-alternate-scroll-lines 1
  "Number of arrow-key events sent per wheel event in Eat alternate screens.")

(defvar sz/eat-normal-state-key "C-."
  "Key used in Eat terminal input modes to enter scrollback navigation.")

(defvar sz/eat-scroll-return-margin 0
  "Extra lines of tolerance before returning to terminal on wheel-down.")

(defun sz/eat-terminal-input-mode-p ()
  "Return non-nil when Eat should own keyboard and cursor input."
  (or (bound-and-true-p eat--semi-char-mode)
      (bound-and-true-p eat--char-mode)))

(defun sz/eat-event-window (event)
  "Return the live window for mouse EVENT."
  (let ((window (posn-window (event-start event))))
    (if (window-live-p window)
        window
      (selected-window))))

(defun sz/eat-wheel-direction (event)
  "Return the logical scroll direction for wheel EVENT."
  (pcase (event-basic-type event)
    ((or 'wheel-up 'mouse-4) 'up)
    ((or 'wheel-down 'mouse-5) 'down)))

(defun sz/eat-live-cursor-visible-p (&optional window)
  "Return non-nil when Eat's live cursor is visible in WINDOW."
  (let ((window (or window (selected-window))))
    (when (and (window-live-p window)
               (eq (window-buffer window) (current-buffer))
               (derived-mode-p 'eat-mode)
               eat-terminal)
      (let ((cursor (eat-term-display-cursor eat-terminal)))
        (or (pos-visible-in-window-p cursor window)
            (>= (window-end window t)
                (save-excursion
                  (goto-char cursor)
                  (line-end-position
                   (1+ (max 0 sz/eat-scroll-return-margin))))))))))

(defun sz/eat-scrollback-mode ()
  "Enter Eat scrollback navigation mode.

This switches Eat to `emacs-mode'.  The Meow state is changed by
`sz/meow-sync-eat-state', so keyboard and mouse navigation stay tied to the
actual Eat input mode."
  (interactive)
  (unless (and (derived-mode-p 'eat-mode)
               (bound-and-true-p eat-terminal))
    (user-error "No Eat terminal in current buffer"))
  (eat-emacs-mode))

(defun sz/eat-scroll-alternate-screen (event)
  "Handle wheel EVENT in Eat terminal input modes.

Many terminal emulators translate mouse-wheel events to Up/Down keys when a
pager uses the alternate screen but has not enabled mouse tracking.  Eat does
not provide that fallback itself, so do it here for tools like `less' and
`git log'.

Outside the alternate screen, wheel-up leaves terminal input and enters
scrollback navigation."
  (interactive "e")
  (let ((window (sz/eat-event-window event)))
    (with-selected-window window
      (with-current-buffer (window-buffer window)
        ;; Alternate-screen programs own their view.  Keep the terminal-style
        ;; wheel fallback there; only normal scrollback enters Meow navigation.
        (if (and (bound-and-true-p eat-terminal)
                 (eat-term-in-alternative-display-p eat-terminal))
            (let ((input (pcase (sz/eat-wheel-direction event)
                           ('up "\e[A")
                           ('down "\e[B"))))
              (when input
                (dotimes (_ sz/eat-alternate-scroll-lines)
                  (eat-term-send-string eat-terminal input))))
          (when (eq (sz/eat-wheel-direction event) 'up)
            (sz/eat-scrollback-mode))
          (mwheel-scroll event))))))

(defun sz/eat-normal-mwheel-scroll (event)
  "Scroll Eat scrollback with EVENT and return to terminal at the live cursor."
  (interactive "e")
  (let ((window (sz/eat-event-window event)))
    (with-selected-window window
      (with-current-buffer (window-buffer window)
        (mwheel-scroll event)
        ;; Do not interrupt mouse selection.  Otherwise dragging near the live
        ;; prompt could deactivate the region by returning to terminal input.
        (when (and (eq (sz/eat-wheel-direction event) 'down)
                   (not (region-active-p))
                   (sz/eat-live-cursor-visible-p window))
          (sz/eat-return-to-terminal))))))

(defun sz/eat-sync-selected-window-cursor (&rest _)
  "Hide the Emacs cursor when an Eat window is detached from the live prompt.

When the selected window is showing an Eat buffer away from the terminal
cursor, hide the Emacs cursor so scrollback does not look like the live
terminal position.  Restore Eat's cursor when the window is back on the
terminal cursor."
  (when (and (derived-mode-p 'eat-mode)
             eat-terminal)
    ;; In scrollback navigation Meow owns the visible cursor.  In terminal input
    ;; modes Eat owns it, and we hide it when the selected window is detached
    ;; from the live terminal cursor.
    (if (not (sz/eat-terminal-input-mode-p))
        (if (bound-and-true-p meow-mode)
            (meow-update-display)
          (setq-local cursor-type t))
      (let* ((selected (selected-window))
             (selected-buffer (window-buffer selected))
             (display-cursor (eat-term-display-cursor eat-terminal))
             (attached
              (and (eq selected-buffer (current-buffer))
                   (= (window-point selected) display-cursor))))
        (if attached
            (eat--set-cursor nil (eat-term-cursor-type eat-terminal))
          (when (bound-and-true-p eat--cursor-blink-mode)
            (eat--cursor-blink-mode -1))
          (setq-local cursor-type nil))))))

(defun sz/eat-snap-to-terminal-cursor (&rest _)
  "Jump point back to Eat's live terminal cursor."
  (when (and (derived-mode-p 'eat-mode)
             eat-terminal)
    (goto-char (eat-term-display-cursor eat-terminal))
    (sz/eat-sync-selected-window-cursor)))

(defun sz/eat-send-shift-return ()
  "Send Shift+Return to the terminal using CSI-u encoding.

This is useful for terminal apps like Codex that request enhanced
keyboard reporting, even when TTY Emacs collapses physical S-RET to RET."
  (interactive)
  (unless eat-terminal
    (user-error "No Eat terminal in current buffer"))
  (eat-term-send-string eat-terminal "\e[13;2u"))

(defun sz/eat-enable-cursor-sync ()
  "Enable local cursor synchronization helpers for Eat."
  (add-hook 'post-command-hook #'sz/eat-sync-selected-window-cursor nil t)
  (add-hook 'eat-update-hook #'sz/eat-sync-selected-window-cursor nil t)
  (sz/eat-sync-selected-window-cursor))

(defvar-local sz/eat-return-terminal-mode 'semi-char
  "Eat input mode to restore when leaving scrollback navigation.")

(defun sz/eat-remember-terminal-mode (&rest _)
  "Remember the current Eat terminal input mode before leaving it."
  (when (derived-mode-p 'eat-mode)
    (cond
     ((bound-and-true-p eat--char-mode)
      (setq-local sz/eat-return-terminal-mode 'char))
     ((bound-and-true-p eat--semi-char-mode)
      (setq-local sz/eat-return-terminal-mode 'semi-char)))))

(defun sz/eat-return-to-terminal ()
  "Return from Eat navigation to the live terminal input position."
  (interactive)
  (unless (and (derived-mode-p 'eat-mode)
               (bound-and-true-p eat-terminal))
    (user-error "No Eat terminal in current buffer"))
  (deactivate-mark)
  (pcase sz/eat-return-terminal-mode
    ('char (eat-char-mode))
    (_ (eat-semi-char-mode)))
  (goto-char (eat-term-display-cursor eat-terminal))
  (when (bound-and-true-p meow-mode)
    (meow-term-mode 1)))

(defun sz/eat-root-or-default-directory ()
  "Return the current Projectile root, or fall back to `default-directory'."
  (or (projectile-project-root)
      default-directory))

(defun sz/run-eat ()
  "Run Eat in the project root or current directory."
  (interactive)
  (let ((default-directory (sz/eat-root-or-default-directory)))
    (call-interactively #'eat)))

(defun sz/run-eat-new-tab ()
  "Open a new tab and run Eat in the project root or current directory."
  (interactive)
  (let* ((root (sz/eat-root-or-default-directory))
         (name (file-name-nondirectory (directory-file-name root))))
    (tab-new)
    (tab-rename (format "eat:%s" name))
    (let ((default-directory root))
      ;; A non-numeric argument tells Eat to create a fresh session instead of
      ;; reusing the default `eat-buffer-name' buffer.
      (eat nil t))))

(use-package eat
  :ensure t
  :bind (("C-c o C-e" . sz/run-eat)
         ("C-c o C-t" . sz/run-eat-new-tab)
         ("M-N" . sz/run-eat-new-tab)
         :map eat-mode-map
         ("M-N" . sz/run-eat-new-tab)
         :map eat-semi-char-mode-map
         ("M-N" . sz/run-eat-new-tab)
         :map eat-char-mode-map
         ("M-N" . sz/run-eat-new-tab))
  :hook ((eat-mode . sz/eat-setup-buffer)
         (eat-mode . sz/eat-enable-cursor-sync))
  :custom
  (eat-enable-mouse t)
  (eat-minimum-latency 0.016)
  (eat-maximum-latency 0.05)
  (eat-term-name "xterm")
  :config
  ;; In terminal input modes, clicks are ignored so they do not move point away
  ;; from the live terminal cursor.  Wheel events are special: normal scrollback
  ;; enters Meow navigation, while alternate-screen programs still receive
  ;; Up/Down fallback events.
  (dolist (map (list eat-semi-char-mode-map eat-char-mode-map))
    (keymap-set map sz/eat-normal-state-key #'sz/eat-scrollback-mode)
    (keymap-set map "C-;" #'sz/meow-term-leader)
    (dolist (key '("<down-mouse-1>" "<mouse-1>" "<drag-mouse-1>"
                   "<down-mouse-2>" "<mouse-2>" "<drag-mouse-2>"
                   "<down-mouse-3>" "<mouse-3>" "<drag-mouse-3>"))
      (keymap-set map key #'sz/eat-ignore-mouse-click))
    (dolist (key '("<wheel-up>" "<wheel-down>" "<mouse-4>" "<mouse-5>"))
      (keymap-set map key #'sz/eat-scroll-alternate-screen)))
  (dolist (map (list eat-mode-map eat-semi-char-mode-map eat-char-mode-map))
    (keymap-set map "S-<return>" #'sz/eat-send-shift-return))
  (dolist (fn '(eat-emacs-mode eat-line-mode))
    (advice-remove fn #'sz/eat-remember-terminal-mode)
    (advice-add fn :before #'sz/eat-remember-terminal-mode))
  (dolist (fn '(eat-char-mode eat-semi-char-mode))
    (advice-remove fn #'sz/eat-snap-to-terminal-cursor)
    (advice-add fn :after #'sz/eat-snap-to-terminal-cursor))
  (eat-eshell-mode)                 ; use Eat to handle term codes in program output
  (eat-eshell-visual-command-mode)) ; commands like less will be handled by Eat

(defun sz/meow-sync-eat-state (&rest _)
  "Keep Meow aligned with the current Eat submode.

Eat mode changes are the source of truth.  This lets normal Eat commands such
as `eat-emacs-mode', `eat-semi-char-mode', and `eat-char-mode' automatically
select the matching Meow state."
  (when (and (derived-mode-p 'eat-mode)
             (bound-and-true-p meow-mode))
    (sz/meow-keypad-only-mode -1)
    (if (or (bound-and-true-p eat--semi-char-mode)
            (bound-and-true-p eat--char-mode))
        (unless (eq (meow--current-state) 'term)
          (meow-term-mode 1))
      (if (bound-and-true-p eat--line-mode)
          (unless (meow-normal-mode-p)
            (meow-normal-mode 1))
        (unless (eq (meow--current-state) 'eat-normal)
          (meow-eat-normal-mode 1))))))

(defun sz/meow-preserve-eat-cursor ()
  "Let Eat manage the cursor while terminal input modes are active."
  (and (derived-mode-p 'eat-mode)
       (sz/eat-terminal-input-mode-p)))

(defun sz/eat-resync-meow-buffers ()
  "Resync Meow state for all live Eat buffers."
  (dolist (buffer (buffer-list))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (derived-mode-p 'eat-mode)
          (sz/meow-sync-eat-state))))))

(defun sz/meow-handle-eat-eshell-mode ()
  "Enable terminal passthrough when `eat-eshell-mode' is active."
  (if (bound-and-true-p eat-eshell-mode)
      (progn
        (meow-mode 1)
        (sz/meow-keypad-only-mode -1)
        (meow-term-mode 1))
    (sz/meow-disable-modal-keep-keypad)))

(with-eval-after-load 'meow
  (defvar sz/meow-term-keymap (make-sparse-keymap)
    "Meow state keymap for terminal passthrough.")

  (defvar sz/meow-term-leader-keymap
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map (or (alist-get 'leader meow-keymap-alist)
                                 mode-specific-map))
      (define-key map (kbd "SPC") #'meow-keypad)
      map)
    "One-shot Meow leader map for terminal buffers.")

  (defun sz/meow-term-leader ()
    "Interpret the next key using Meow leader bindings in terminal buffers."
    (interactive)
    (setq-local meow--indicator
                (propertize " TERM->MEOW " 'face 'meow-keypad-indicator))
    (force-mode-line-update)
    (set-transient-map
     sz/meow-term-leader-keymap
     nil
     (lambda ()
       (meow--update-indicator)
       (force-mode-line-update))))

  (meow-define-state term
    "Passthrough Meow state for terminal-like buffers (leader only)."
    :lighter " [TERM]"
    :keymap sz/meow-term-keymap)

  ;; The state symbol is `term', so Meow's mode-line indicator renders "TERM"
  ;; instead of the old custom state name "TERMINAL".
  (meow-define-keys 'term
    '("C-;" . sz/meow-term-leader)
    '("M-N" . sz/run-eat-new-tab))
  (keymap-set sz/meow-term-keymap "C-;" #'sz/meow-term-leader)
  (keymap-set sz/meow-term-keymap "M-N" #'sz/run-eat-new-tab)

  (defvar sz/meow-eat-normal-keymap
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map meow-normal-state-keymap)
      map)
    "Meow state keymap for Eat scrollback navigation.")

  (meow-define-state eat-normal
    "Meow normal-style state for navigating Eat buffers."
    :lighter " [TERM->MEOW]"
    :keymap sz/meow-eat-normal-keymap)

  ;; `meow-indicator' renders from `meow-replace-state-name-list', not from the
  ;; minor-mode lighter above.  Override Meow's default upcased custom state
  ;; name ("EAT-NORMAL") for the mode line.
  (setf (alist-get 'eat-normal meow-replace-state-name-list)
        "TERM->MEOW")

  (dolist (key '("<escape>"
                 "i" "I" "a" "A"
                 "c" "C" "d" "D" "m" "p"
                 "r" "R" "s" "S" "u" "U"))
    (keymap-set sz/meow-eat-normal-keymap key #'sz/eat-return-to-terminal))

  (dolist (key '("<wheel-up>" "<wheel-down>" "<mouse-4>" "<mouse-5>"))
    (keymap-set sz/meow-eat-normal-keymap key #'sz/eat-normal-mwheel-scroll))

  (keymap-set sz/meow-eat-normal-keymap "C-;" #'meow-M-x)
  (keymap-set sz/meow-eat-normal-keymap "M-N" #'sz/run-eat-new-tab)

  (setq meow-update-cursor-functions-alist
        (assq-delete-all 'sz/meow-preserve-eat-cursor
                         meow-update-cursor-functions-alist))
  (add-to-list 'meow-update-cursor-functions-alist
               '(sz/meow-preserve-eat-cursor . ignore))

  (setq meow-mode-state-list
        (assq-delete-all 'eat-mode meow-mode-state-list))
  (add-to-list 'meow-mode-state-list '(eat-mode . term))

  (with-eval-after-load 'eat
    (remove-hook 'eat-mode-hook #'sz/meow-sync-eat-state)
    (add-hook 'eat-mode-hook #'sz/meow-sync-eat-state)
    (dolist (fn '(eat-emacs-mode
                  eat-semi-char-mode
                  eat-char-mode
                  eat-line-mode))
      (advice-remove fn #'sz/meow-sync-eat-state)
      (advice-add fn :after #'sz/meow-sync-eat-state))
    (sz/eat-resync-meow-buffers)))
