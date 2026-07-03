;; === Eat terminal
;;
;; Minimal Meow/Eat bridge:
;; - Meow insert means Eat terminal input (`semi-char' by default).
;; - Meow normal means Eat `emacs-mode' for scrollback navigation/copying.
;; - `ESC' leaves terminal input; `C-;' sends a literal ESC.

(defcustom sz/eat-width-safety-margin 2
  "Columns to keep unused when reporting Eat's PTY width.

Some full-screen TUIs pad styled rows to the reported terminal width.  Eat can
render those rows as wrapped when Emacs and the TUI disagree by one or two
display cells (for example on rows with Unicode status glyphs, or near the
right fringe/scrollbar).  Reporting a slightly smaller PTY width keeps those
full-width rows away from Eat's wrap edge."
  :type 'integer
  :group 'eat)

(defun sz/eat-adjust-process-window-size-largest-safe (process windows)
  "Like `window-adjust-process-window-size-largest', but leave a width margin.

PROCESS and WINDOWS are passed through to
`window-adjust-process-window-size-largest'.  The height is unchanged; only the
reported column count is reduced by `sz/eat-width-safety-margin'."
  (when-let ((size (window-adjust-process-window-size-largest process windows)))
    (cons (max 1 (- (car size) sz/eat-width-safety-margin))
          (cdr size))))

(defcustom sz/eat-follow-output-in-input t
  "When non-nil, Eat terminal input modes keep visible windows at the live cursor.

Eat only follows output while point/window-point already match the terminal
cursor.  Modal state changes, mouse clicks, or accidental navigation can leave
point in scrollback while the terminal is still in input mode, which makes TUIs
continue rendering off-screen.  This option snaps back to the live terminal
view whenever output arrives or terminal input mode is re-entered."
  :type 'boolean
  :group 'eat)

(defun sz/eat-setup-buffer ()
  "Apply local UI rules for Eat buffers."
  ;; Avoid stale/small PTY sizes when an Eat buffer is shown in more than one
  ;; window.  A too-small terminal height makes pagers render only part of the
  ;; selected window, leaving blank space below.
  ;;
  ;; Also keep a small spare-column margin: Pi pads autocomplete/status rows to
  ;; the full PTY width, and Eat can immediately wrap such rows into an extra
  ;; blank line or clip the right status text.
  (setq-local window-adjust-process-window-size-function
              #'sz/eat-adjust-process-window-size-largest-safe)
  ;; If point is moved to the live terminal cursor from off-screen, keep Emacs
  ;; from recentering it heuristically before our explicit follow logic runs.
  (setq-local scroll-conservatively 101))

(defcustom sz/meow-eat-input-mode 'semi-char
  "Eat input mode used when entering Meow insert state."
  :type '(choice
          (const :tag "Semi-char mode" semi-char)
          (const :tag "Char mode" char)
          (const :tag "Line mode" line))
  :group 'eat)

(defcustom sz/meow-eat-start-in-input t
  "When non-nil, new Eat buffers start in terminal input/Meow insert state."
  :type 'boolean
  :group 'eat)

(defun sz/meow-eat-ready-p ()
  "Return non-nil when the current buffer has an Eat terminal."
  (and (derived-mode-p 'eat-mode)
       (bound-and-true-p eat-terminal)))

(defun sz/eat-terminal-input-mode-p ()
  "Return non-nil when Eat should behave like a live terminal."
  (or (bound-and-true-p eat--semi-char-mode)
      (bound-and-true-p eat--char-mode)))

(defun sz/eat-follow-live-output (&rest _)
  "Keep the selected Eat window attached to the live terminal in input mode.

Use Eat's own scroll synchronizer instead of setting `window-start' directly:
Eat keeps long logical terminal lines and wrapped display rows in sync there,
and bypassing it can corrupt TUI redraws."
  (when (and sz/eat-follow-output-in-input
             (sz/meow-eat-ready-p)
             (sz/eat-terminal-input-mode-p)
             (not (region-active-p))
             (fboundp 'eat--synchronize-scroll-windows))
    (funcall eat--synchronize-scroll-function
             (eat--synchronize-scroll-windows 'force-selected))))

(defun sz/eat-send-string (string)
  "Send STRING directly to the Eat terminal."
  (unless (and (derived-mode-p 'eat-mode)
               (bound-and-true-p eat-terminal))
    (user-error "No Eat terminal in current buffer"))
  (eat-term-send-string eat-terminal string))

(defun sz/eat-send-escape ()
  "Send a literal ESC to the Eat terminal."
  (interactive)
  (sz/eat-send-string "\e"))

(defun sz/eat-send-shift-return ()
  "Send Shift+Return to the terminal using CSI-u encoding.

This is useful for terminal apps like Codex that request enhanced
keyboard reporting, even when TTY Emacs collapses physical S-RET to RET."
  (interactive)
  (sz/eat-send-string "\e[13;2u"))

(defun sz/meow-eat-enter-input ()
  "Let Eat receive terminal input when Meow enters insert state.
Return non-nil when Eat accepted the input-mode switch."
  (when (sz/meow-eat-ready-p)
    (pcase sz/meow-eat-input-mode
      ('char (eat-char-mode))
      ('line (eat-line-mode))
      (_ (eat-semi-char-mode)))
    (sz/eat-follow-live-output)
    t))

(defun sz/meow-eat-enter-navigation ()
  "Use Eat `emacs-mode' when Meow leaves insert state."
  (when (derived-mode-p 'eat-mode)
    (eat-emacs-mode)
    t))

(defun sz/meow-eat-exit-terminal-input ()
  "Leave Eat terminal input and enter Meow normal navigation."
  (interactive)
  (sz/meow-eat-enter-navigation)
  (when (fboundp 'meow-normal-mode)
    (meow-normal-mode 1)))

(defun sz/meow-eat-ensure-terminal-input (&rest _)
  "Enter terminal input once Eat's process is ready."
  (when (and (derived-mode-p 'eat-mode)
             (fboundp 'meow-insert-mode)
             (sz/meow-eat-enter-input))
    (meow-insert-mode 1)
    (remove-hook 'eat-update-hook #'sz/meow-eat-ensure-terminal-input t)))

(defun sz/meow-eat-setup ()
  "Set up minimal buffer-local Meow/Eat state synchronization."
  (when (fboundp 'meow-mode)
    (meow-mode 1)
    (add-hook 'meow-insert-enter-hook #'sz/meow-eat-enter-input nil t)
    (add-hook 'meow-insert-exit-hook #'sz/meow-eat-enter-navigation nil t)
    (if sz/meow-eat-start-in-input
        (add-hook 'eat-update-hook #'sz/meow-eat-ensure-terminal-input nil t)
      (sz/meow-eat-enter-navigation)
      (meow-normal-mode 1))))

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
         ("C-c t C-e" . sz/run-eat-new-tab)
         ("M-N" . sz/run-eat-new-tab)
         :map eat-mode-map
         ("M-N" . sz/run-eat-new-tab)
         ("S-<return>" . sz/eat-send-shift-return)
         ("C-;" . sz/eat-send-escape)
         :map eat-semi-char-mode-map
         ("M-N" . sz/run-eat-new-tab)
         ("<escape>" . sz/meow-eat-exit-terminal-input)
         ("C-." . sz/meow-eat-exit-terminal-input)
         ("C-;" . sz/eat-send-escape)
         ("S-<return>" . sz/eat-send-shift-return)
         :map eat-char-mode-map
         ("M-N" . sz/run-eat-new-tab)
         ("<escape>" . sz/meow-eat-exit-terminal-input)
         ("C-." . sz/meow-eat-exit-terminal-input)
         ("C-;" . sz/eat-send-escape)
         ("S-<return>" . sz/eat-send-shift-return)
         :map eat-line-mode-map
         ("M-N" . sz/run-eat-new-tab)
         ("<escape>" . sz/meow-eat-exit-terminal-input)
         ("C-." . sz/meow-eat-exit-terminal-input)
         ("C-;" . sz/eat-send-escape)
         ("S-<return>" . sz/eat-send-shift-return))
  :hook ((eat-mode . sz/eat-setup-buffer)
         (eat-mode . sz/meow-eat-setup)
         (eat-update . sz/eat-follow-live-output))
  :custom
  ;; Some TUIs request Eat's "very visible" blinking cursor states.  Eat
  ;; implements those with its own timer by toggling `cursor-type', which can
  ;; look glitchy when combined with Emacs/Meow cursor updates.  Keep the
  ;; requested cursor shapes, but make them non-blinking.
  (eat-very-visible-cursor-type '(box nil nil))
  (eat-very-visible-vertical-bar-cursor-type '(bar nil nil))
  (eat-very-visible-horizontal-bar-cursor-type '(hbar nil nil))
  (eat-minimum-latency 0.016)
  (eat-maximum-latency 0.05)
  (eat-term-name "xterm")
  :config
  (dolist (fn '(eat-char-mode eat-semi-char-mode))
    (advice-remove fn #'sz/eat-follow-live-output)
    (advice-add fn :after #'sz/eat-follow-live-output))
  (eat-eshell-mode)                 ; use Eat to handle term codes in program output
  (eat-eshell-visual-command-mode)) ; commands like less will be handled by Eat
