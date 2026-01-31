;; === Package Archives
(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

(setopt initial-major-mode 'fundamental-mode)
(setopt display-time-default-load-average nil)

(setopt auto-revert-avoid-polling t)
(setopt auto-revert-interval 1)
(setopt auto-revert-check-vc-info t)
(global-auto-revert-mode)

(savehist-mode)

;; Move through windows with Ctrl-<arrow keys>
(windmove-default-keybindings 'control) ; You can use other modifiers here

(setopt sentence-end-double-space nil)

;; Make right-click do something sensible
(when (display-graphic-p)
  (context-menu-mode))

(setq make-backup-files nil)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(setopt enable-recursive-minibuffers t)
(setopt completion-cycle-threshold 1)
(setopt completions-detailed t)
(setopt tab-always-indent 'complete)
(setopt completion-styles '(basic initials substring))

(setopt completion-auto-help 'always)
(setopt completions-max-height 20)
(setopt completions-format 'one-column)
(setopt completions-group t)
(setopt completion-auto-select 'second-tab)
;(setopt completion-auto-select t)                     ; See `C-h v completion-auto-select' for more possible values

(keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete) ; TAB acts more like how it does in the shell

(setopt line-number-mode t)
(setopt column-number-mode t)

(setopt x-underline-at-descent-line nil)
(setopt switch-to-buffer-obey-display-actions t)

(setopt show-trailing-whitespace nil)
(setopt indicate-buffer-boundaries 'left)

(setopt mouse-wheel-tilt-scroll t)
(setopt mouse-wheel-flip-direction t)

(setopt indent-tabs-mode nil)
(setopt tab-width 4)

(blink-cursor-mode t)
(setopt blink-cursor-interval 0.7)

(pixel-scroll-precision-mode)

(cua-mode)

(xterm-mouse-mode 1)

(setopt display-line-numbers-width 3)

(let ((text-hooks '(text-mode-hook prog-mode-hook)))
  (mapc (lambda (hook)
	  (add-hook hook 'hl-line-mode)
	  (add-hook hook 'display-line-numbers-mode)
	  (add-hook hook 'visual-line-mode))
	text-hooks))

(setopt tab-bar-show 1)

(add-to-list 'tab-bar-format 'tab-bar-format-align-right 'append)
(add-to-list 'tab-bar-format 'tab-bar-format-global 'append)
(setopt display-time-format "%a %F %T")
(setopt display-time-interval 1)
(display-time-mode)

(add-to-list 'default-frame-alist '(font . "Iosevka-13" ))
(set-face-attribute 'default t :font "Iosevka-13")

;; === Packages

(use-package modus-themes
  :ensure t
  :config
  (setq shr-use-fonts nil)
  (load-theme 'modus-operandi-deuteranopia :no-confirm)
  (when (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (_f) (enable-theme 'modus-operandi-deuteranopia)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(cape claude-code consult corfu-terminal eat elfeed elfeed-org
          json-mode kind-icon magit markdown-mode meow modus-themes
          mu4e nix-mode orderless super-save telega tempel vertico
          wgrep yaml-mode))
 '(package-vc-selected-packages
   '((claude-code :url "https://github.com/stevemolitor/claude-code.el"))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(use-package consult
  :ensure t
  :bind (
         ;; Drop-in replacements
         ("C-x b" . consult-buffer)     ; orig. switch-to-buffer
         ("M-y"   . consult-yank-pop)   ; orig. yank-pop
         ;; Searching
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)       ; Alternative: rebind C-s to use
         ("M-s s" . consult-line)       ; consult-line instead of isearch, bind
         ("M-s L" . consult-line-multi) ; isearch to M-s s
         ("M-s o" . consult-outline)
         ;; Isearch integration
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)   ; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history) ; orig. isearch-edit-string
         ("M-s l" . consult-line)            ; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)      ; needed by consult-line to detect isearch
         )
  :config
  (setq consult-narrow-key "<"))

(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package vertico-directory
  :ensure nil
  :after vertico
  :bind (:map vertico-map
              ("M-DEL" . vertico-directory-delete-word)))

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :bind
  (:map corfu-map
        ("SPC" . corfu-insert-separator)
        ("C-n" . corfu-next)
        ("C-p" . corfu-previous)))

(use-package corfu-popupinfo
  :after corfu
  :ensure nil
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.25 . 0.1))
  (corfu-popupinfo-hide nil)
  :config
  (corfu-popupinfo-mode))

(use-package corfu-terminal
  :if (not (display-graphic-p))
  :ensure t
  :config
  (corfu-terminal-mode))

(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package kind-icon
  :if (display-graphic-p)
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package eshell
  :init
  (defun bedrock/setup-eshell ()
    ;; Something funny is going on with how Eshell sets up its keymaps; this is
    ;; a work-around to make C-r bound in the keymap
    (keymap-set eshell-mode-map "C-r" 'consult-history))
  :hook ((eshell-mode . bedrock/setup-eshell)))

(defun my/eat-hide-emacs-cursor ()
  (setq-local cursor-type nil
              cursor-in-non-selected-windows nil))

(defun my/eat-hide-emacs-cursor-enable ()
  (my/eat-hide-emacs-cursor)
  (add-hook 'post-command-hook #'my/eat-hide-emacs-cursor nil t))

(use-package eat
  :ensure t
  :hook
  (eat-mode . my/eat-hide-emacs-cursor-enable)
  
  :custom
  (eat-term-name "xterm")
  
  :config
  (eat-eshell-mode)                     ; use Eat to handle term codes in program output
  (eat-eshell-visual-command-mode))     ; commands like less will be handled by Eat

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless)))

(use-package wgrep
  :ensure t
  :config
  (setq wgrep-auto-save-buffer t))

(use-package emacs
  :config
  ;; Tell Emacs to prefer the treesitter mode
  ;; You'll want to run the command `M-x treesit-install-language-grammar' before editing.
  (setq major-mode-remap-alist
        '((yaml-mode . yaml-ts-mode)
          (bash-mode . bash-ts-mode)
          (js2-mode . js-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (json-mode . json-ts-mode)
          (css-mode . css-ts-mode)
          (python-mode . python-ts-mode)))
  :hook
  ((prog-mode . electric-pair-mode)))

(use-package project
  :custom
  (when (>= emacs-major-version 30)
    (project-mode-line t)))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package markdown-mode
  :ensure t
  :hook ((markdown-mode . visual-line-mode)))

(use-package yaml-mode
  :ensure t)

(use-package json-mode
  :ensure t)

(use-package eglot
  :custom
  (eglot-send-changes-idle-time 0.1)
  (eglot-extend-to-xref t)

  :config
  (fset #'jsonrpc--log-event #'ignore)
  )

(use-package tempel
  :ensure t
  :bind (("M-*" . tempel-insert)
         ("M-+" . tempel-complete)
         :map tempel-map
         ("C-c RET" . tempel-done)
         ("C-<down>" . tempel-next)
         ("C-<up>" . tempel-previous)
         ("M-<down>" . tempel-next)
         ("M-<up>" . tempel-previous))
  :init
  ;; Make a function that adds the tempel expansion function to the
  ;; list of completion-at-point-functions (capf).
  (defun tempel-setup-capf ()
    (add-hook 'completion-at-point-functions #'tempel-expand -1 'local))
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf))

;; === Org Mode

(setq org-directory "~/org")
(setq org-agenda-files '("~/org"))
(setq org-archive-location "~/org/archive/%s::")

(defun my/meow-org-promote-subtree ()
  (interactive)
  (when (derived-mode-p 'org-mode) (org-promote-subtree)))

(defun my/meow-org-demote-subtree ()
  (interactive)
  (when (derived-mode-p 'org-mode) (org-demote-subtree)))

(defun my/meow-org-move-subtree-down ()
  (interactive)
  (when (derived-mode-p 'org-mode) (org-move-subtree-down)))

(defun my/meow-org-move-subtree-up ()
  (interactive)
  (when (derived-mode-p 'org-mode) (org-move-subtree-up)))

(use-package org
  :hook
  ((org-mode . visual-line-mode)
   (org-agenda-mode . hl-line-mode)
   (org-mode . flyspell-mode)
   (org-mode . org-indent-mode))

  :bind
  ("C-c j h" . org-agenda)
  ("C-c j j" . org-capture)

  :config
  ;; Make org-open-at-point follow file links in the same window
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)

  ;; TODO: debug it with my org files (CANX state?)
  (setq org-element-use-cache nil)

  (setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "STRT(s!)" "WAIT(w!)" "|" "DONE(d)" "CANX(c)")))

  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-use-outline-path 'file)
  (setq org-link-descriptive nil)
  
  (setq org-log-into-drawer "LOGBOOK")
  (setq org-log-repeat 'time)
  (setq org-log-done 'time)

  (setq org-image-actual-width 400)
  (setq org-startup-with-inline-images t)

  (setq org-blank-before-new-entry
		'((heading . nil)
		  (plain-list-item . auto)))
  
  (setq org-startup-folded 'nofold)
  (setq org-indent-indentation-per-level 2)

  (setopt org-capture-templates
		  '(("i" "inbox item" item (file "inbox.org")
			 "%?\n%i")

			("m" "my todo" entry (file+olp "areas.org" "Personal" "Tasks")
   			 "* TODO %?")

			("t" "today todo" entry (file+olp "areas.org" "Personal" "Tasks")
   			 "* TODO %?\nSCHEDULED: %t")

			("b" "timeblock" entry (file+olp "areas.org" "Personal" "Timeblocks")
   			 "* %^{TIMEBLOCK}  :timeblock:\n%^{TIMESTAMP}T"
   			 :immediate-finish t)

			("w" "Work")
			("wb" "timeblock" entry (file+olp "health-samurai.org" "Work" "Timeblocks")
   			 "* %^{TIMESTAMP}T WORK  :timeblock:"
   			 :immediate-finish t
			 :empty-lines 1)))

   (setopt org-agenda-custom-commands
   		  '(;; Archive tasks
   			("#" "To archive" todo "DONE|CANX")

   			;; Review weekly appointements
   			("$" "Weekly appointments" agenda* "Weekly appointments")

   			;; Review weekly tasks
   			("w" "Week tasks" agenda "Scheduled tasks for this week"
   			 (; (org-agenda-category-filter-preset '("-RDV")) ; RDV for Rendez-vous
			  (org-agenda-tag-filter-preset '("-habit"))
   			  (org-deadline-warning-days 0)
   			  (org-agenda-use-time-grid nil)))

			;; Review daily tasks
			("j" "Today Journal"
			 ((agenda ""
				  ((org-agenda-span 1)
				   (org-agenda-start-day "0d")
				   (org-agenda-use-time-grid t)
				   (org-agenda-time-grid '((daily today require-timed)
							   (1000 1500 2000 0000)
							   "......" "----------------"))
				   (org-agenda-overriding-header "Timeline / Schedule")))
			  (todo "STRT"
				((org-agenda-overriding-header "In progress")))
			  (todo "NEXT"
				((org-agenda-overriding-header "Next actions")))
			  (todo "WAITING"
				((org-agenda-overriding-header "Waiting / blocked")))))

   			;; Review started and next tasks
   			("s" "STRT/NEXT" tags-todo "TODO={STRT\\|NEXT}")

   			;; Review other non-scheduled/deadlined to-do tasks
   			("t" "TODO" tags-todo "TODO={TODO}+DEADLINE=\"\"+SCHEDULED=\"\"")

   			;; Review other non-scheduled/deadlined pending tasks
   			("w" "WAIT" tags-todo "TODO={WAIT}+DEADLINE=\"\"+SCHEDULED=\"\"")

   			;; Review upcoming deadlines for the next 60 days
   			("!" "Deadlines all" agenda "Past/upcoming deadlines"
   			 ((org-agenda-span 1)
   			  (org-deadline-warning-days 60)
   			  (org-agenda-entry-types '(:deadline))))))
  )

(defun my-org-agenda-save-and-redo (&optional arg)
    "Save all Org buffers, then redo the agenda.
ARG is passed to `org-agenda-redo-all'."
    (interactive "P")
    (org-save-all-org-buffers)
    (org-agenda-redo-all arg))

(use-package org-agenda
  :after org
  :bind (:map org-agenda-mode-map
              ("g" . my-org-agenda-save-and-redo))

  :config
  (define-advice org-agenda (:around (orig-fun &rest args) split-vertically)
    (let ((split-width-threshold 80))
      (apply orig-fun args))))

(use-package super-save
  :ensure t
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t)
  (setq auto-save-default nil))

(use-package inheritenv
  :vc (:url "https://github.com/purcell/inheritenv" :rev :newest))

(use-package claude-code :ensure t
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
  :config

  (claude-code-mode)
  :bind-keymap ("C-c c" . claude-code-command-map)

  :bind
  (:repeat-map my-claude-code-map ("M" . claude-code-cycle-mode)))

(use-package elfeed
  :ensure t)

(use-package elfeed-org
  :ensure t
  :config
  (setq rmh-elfeed-org-files (list "~/org/rss.org"))
  (elfeed-org))

(use-package nix-mode
  :ensure t
  :mode "\\.nix^\\'"
  :hook (before-save . nix-format-before-save))

;; === Modal Editing

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
  
  :config
  (meow-setup)
  
  (meow-leader-define-key '("<SPC>" . meow-M-x))
  
  (keymap-global-set "M-l" #'windmove-right)
  (keymap-global-set "M-h" #'windmove-left)
  (keymap-global-set "M-k" #'windmove-up)
  (keymap-global-set "M-j" #'windmove-down)

  (keymap-set meow-normal-state-keymap "C-h" #'my/meow-org-promote-subtree)
  (keymap-set meow-normal-state-keymap "C-l" #'my/meow-org-demote-subtree)
  (keymap-set meow-normal-state-keymap "C-j" #'my/meow-org-move-subtree-down)
  (keymap-set meow-normal-state-keymap "C-k" #'my/meow-org-move-subtree-up)
  
  (meow-global-mode 1)

  (add-hook 'meow-insert-exit-hook 'corfu-quit)
  (setopt meow-use-clipboard t)
  (setopt meow-expand-hint-counts
          '((word . 0)
            (line . 30)
            (block . 30)
            (find . 30)
            (till . 30)))

  (add-to-list 'meow-mode-state-list '(eat-mode . insert))
  (add-to-list 'meow-mode-state-list '(eat-eshell-mode . insert))
  
  (meow-setup-indicator))

;; === Custom Functions

(require 'org)

(defun my/org-waybar-current-timeblock ()
  "Return heading of the timeblock covering now, or nil."
  (let* ((now  (current-time))
         (date (format-time-string "%Y-%m-%d" now))
         (hm   (format-time-string "%H:%M" now))
         (re   (concat "<" date "[^>]* \\([0-9][0-9]:[0-9][0-9]\\)-\\([0-9][0-9]:[0-9][0-9]\\)>")))
    (catch 'done
      (dolist (f (org-agenda-files))
        (with-current-buffer (find-file-noselect f)
          (org-with-wide-buffer
            (goto-char (point-min))
            (while (re-search-forward re nil t)
              (let ((s (match-string 1)) (e (match-string 2)))
                (when (and (not (string< hm s))  ; s <= hm
                           (string< hm e))        ; hm < e
                  (org-back-to-heading t)
                  (throw 'done (substring-no-properties (org-get-heading t t t t)))))))))
      nil)))

;; === GC

(setq gc-cons-threshold 800000)
