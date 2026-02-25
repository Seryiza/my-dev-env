;; === Modal editing

(autoload 'sz/meow-org-promote-subtree "sz-org" nil t)
(autoload 'sz/meow-org-demote-subtree "sz-org" nil t)
(autoload 'sz/meow-org-move-subtree-down "sz-org" nil t)
(autoload 'sz/meow-org-move-subtree-up "sz-org" nil t)

(defvar sz/meow-keypad-only-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c SPC") #'meow-keypad)
    map)
  "Keymap for `sz/meow-keypad-only-mode'.")

(define-minor-mode sz/meow-keypad-only-mode
  "Expose Meow keypad in buffers where Meow modal editing is disabled."
  :init-value nil
  :lighter ""
  :keymap sz/meow-keypad-only-mode-map)

(defun sz/meow-term-enable ()
  "Enable Meow in `term-mode' buffers."
  (meow-mode 1))

(defun sz/meow-term-line ()
  "Use NORMAL state in `term-line-mode'."
  (when (bound-and-true-p meow-mode)
    (meow-normal-mode 1)))

(defun sz/meow-term-char ()
  "Use INSERT state in `term-char-mode'."
  (when (bound-and-true-p meow-mode)
    (meow-insert-mode 1)))

(defun sz/meow-disable-modal-keep-keypad ()
  "Disable Meow modal states here, but keep keypad available."
  (meow-mode -1)
  (sz/meow-keypad-only-mode 1))

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore)))

(use-package meow
  :ensure t
  :demand t
  :hook (meow-insert-exit . corfu-quit)
  :config
  (meow-setup)

  (meow-leader-define-key '("<SPC>" . meow-M-x))
  (meow-leader-define-key '("C-;" . meow-M-x))
  (meow-leader-define-key '("s" . save-buffer))

  (keymap-set meow-normal-state-keymap "C-h" #'sz/meow-org-promote-subtree)
  (keymap-set meow-normal-state-keymap "C-l" #'sz/meow-org-demote-subtree)
  (keymap-set meow-normal-state-keymap "C-j" #'sz/meow-org-move-subtree-down)
  (keymap-set meow-normal-state-keymap "C-k" #'sz/meow-org-move-subtree-up)

  (meow-global-mode 1)

  (setopt meow-use-clipboard t)
  (setopt meow-expand-hint-counts
          '((word . 0)
            (line . 30)
            (block . 30)
            (find . 30)
            (till . 30)))

  (add-hook 'term-mode-hook #'sz/meow-term-enable)
  (add-hook 'term-line-mode-hook #'sz/meow-term-line)
  (add-hook 'term-char-mode-hook #'sz/meow-term-char)

  (dolist (hook '(shell-mode-hook
                  eshell-mode-hook
                  eat-mode-hook
                  eat-eshell-mode-hook
                  agent-shell-mode-hook))
    (add-hook hook #'sz/meow-disable-modal-keep-keypad))

  (meow-setup-indicator)

  (defvar my/meow-vterm-keymap (make-sparse-keymap)
    "Meow state keymap for vterm passthrough.")

  (defvar my/meow-vterm-leader-keymap
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map (or (alist-get 'leader meow-keymap-alist)
                                 mode-specific-map))
      (define-key map (kbd "SPC") #'meow-keypad)
      map)
    "One-shot Meow leader map for vterm.")

  (defun my/meow-vterm-leader ()
    "Interpret the next key using Meow leader bindings in vterm."
    (interactive)
    (setq-local meow--indicator (propertize " VTERM->MEOW " 'face 'meow-keypad-indicator))
    (force-mode-line-update)
    (set-transient-map
     my/meow-vterm-leader-keymap
     nil
     (lambda ()
       (meow--update-indicator)
       (force-mode-line-update))))

  (meow-define-state vterm
    "Passthrough Meow state for vterm (only leader trigger)."
    :lighter " [VT]"
    :keymap my/meow-vterm-keymap)

  ;; Only bind the leader trigger in this state
  (meow-define-keys 'vterm
    '("C-;" . my/meow-vterm-leader))

  ;; Start vterm buffers in this custom state
  (add-to-list 'meow-mode-state-list '(vterm-mode . vterm)))
