;; === Base options

(setopt initial-major-mode 'fundamental-mode)
(setopt display-time-default-load-average nil)

(setopt auto-revert-avoid-polling t)
(setopt auto-revert-use-notify t)
(setopt auto-revert-interval 5)
(setopt auto-revert-check-vc-info nil)
(setopt auto-revert-verbose nil)
(global-auto-revert-mode)

(savehist-mode)

;; Move through windows with Ctrl-<arrow keys>
(windmove-default-keybindings 'control)

(setopt sentence-end-double-space nil)

;; Make right-click do something sensible
(when (and (display-graphic-p) (fboundp 'context-menu-mode))
  (context-menu-mode))

(setopt make-backup-files t)
(let ((backup-dir (locate-user-emacs-file "backups/")))
  (make-directory backup-dir t)
  (setq backup-directory-alist `(("." . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,backup-dir t))
        backup-by-copying t
        delete-old-versions t
        kept-new-versions 6
        kept-old-versions 2
        version-control t))

(use-package which-key
  :ensure t
  :config (which-key-mode))

(setopt enable-recursive-minibuffers t)
(setopt completion-cycle-threshold 1)
(setopt completions-detailed t)
(setopt tab-always-indent 'complete)
(setopt completion-auto-help 'always)
(setopt completions-max-height 20)
(setopt completions-format 'one-column)
(setopt completions-group t)
(setopt completion-auto-select 'second-tab)
;; (setopt completion-auto-select t) ; See `C-h v completion-auto-select' for more possible values

;; TAB acts more like how it does in the shell
(keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete)

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

(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode))

(unless (display-graphic-p)
  (xterm-mouse-mode 1))

(setopt display-line-numbers-width 3)

(setopt display-time-format "%a %F %T")
(setopt display-time-interval 1)
(display-time-mode)

;; Default frame configuration
(setq frame-resize-pixelwise t)
(tool-bar-mode -1) ; All these tools are in the menu-bar anyway
(setq default-frame-alist '((fullscreen . maximized)

                            ;; You can turn off scroll bars by uncommenting these lines:
                            ;; (vertical-scroll-bars . nil)
                            ;; (horizontal-scroll-bars . nil)
                            ))

(defconst sz/default-font-family "Iosevka Slab")
(defconst sz/default-font-size 12)

(defun sz/apply-default-font (&optional frame)
  (with-selected-frame (or frame (selected-frame))
    (when (display-graphic-p)
      (set-face-attribute 'default nil
                          :family sz/default-font-family
                          :height (* 10 sz/default-font-size)
                          :weight 'normal
                          :width 'condensed))))

(add-to-list 'default-frame-alist
             `(font . ,(format "%s-%d" sz/default-font-family sz/default-font-size)))
(sz/apply-default-font)

;; === Packages

;; Dired is built-in, so use :ensure nil.
(use-package dired
  :ensure nil
  :config
  ;; Must include "-l" per the manual.
  (setq dired-listing-switches "-al --group-directories-first"))

(use-package modus-themes
  :ensure t
  :pin gnu
  :hook (after-make-frame-functions . (lambda (_f)
                                        (when (daemonp)
                                          (enable-theme 'modus-operandi-deuteranopia))))
  :config
  (setq shr-use-fonts nil)
  (load-theme 'modus-operandi-deuteranopia :no-confirm))

(use-package consult
  :ensure t
  :bind (("C-x b" . consult-buffer)     ; orig. switch-to-buffer
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
         ("M-s L" . consult-line-multi))     ; needed by consult-line to detect isearch
  :config (setq consult-narrow-key "<"))

(use-package vertico
  :ensure t
  :init (vertico-mode))

(use-package vertico-directory
  :ensure nil
  :after vertico
  :bind (:map vertico-map ("M-DEL" . vertico-directory-delete-word)))

(use-package corfu
  :ensure t
  :init (global-corfu-mode 1)
  :bind
  (:map corfu-map
        ("C-n" . corfu-next)
        ("C-p" . corfu-previous))
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.05)
  (corfu-auto-trigger "."))

(use-package corfu-popupinfo
  :after corfu
  :ensure nil
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.25 . 0.1))
  (corfu-popupinfo-hide nil)
  :config (corfu-popupinfo-mode))

(use-package corfu-terminal
  :if (not (display-graphic-p))
  :ensure t
  :config (corfu-terminal-mode))

(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package kind-icon
  :if (display-graphic-p)
  :ensure t
  :after corfu
  :config (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package eshell
  :init
  (defun bedrock/setup-eshell ()
    ;; Something funny is going on with how Eshell sets up its keymaps; this is
    ;; a work-around to make C-r bound in the keymap
    (keymap-set eshell-mode-map "C-r" 'consult-history))
  :hook (eshell-mode . bedrock/setup-eshell))

(defun sz/eat-hide-emacs-cursor ()
  (setq-local cursor-type nil
              cursor-in-non-selected-windows nil))

(defun sz/eat-hide-emacs-cursor-enable ()
  (sz/eat-hide-emacs-cursor)
  (add-hook 'post-command-hook #'sz/eat-hide-emacs-cursor nil t))

(use-package eat
  :ensure t
  :hook (eat-mode . sz/eat-hide-emacs-cursor-enable)
  :custom (eat-term-name "xterm")
  :config
  (eat-eshell-mode)                 ; use Eat to handle term codes in program output
  (eat-eshell-visual-command-mode)) ; commands like less will be handled by Eat

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-pcm-leading-wildcard t))

(use-package wgrep
  :ensure t
  :config (setq wgrep-auto-save-buffer t))

(use-package emacs
  :hook
  ((before-save . whitespace-cleanup)
   (prog-mode . electric-pair-mode)
   (text-mode . hl-line-mode)
   (prog-mode . hl-line-mode)
   (text-mode . display-line-numbers-mode)
   (prog-mode . display-line-numbers-mode)
   (text-mode . visual-line-mode)
   (after-make-frame-functions . sz/apply-default-font))
  :config
  (setq show-paren-delay 0)
  (show-paren-mode 1)

  (setopt treesit-language-source-alist
          '((css "https://github.com/tree-sitter/tree-sitter-css")))

  ;; Tell Emacs to prefer the treesitter mode
  ;; You'll want to run the command `M-x treesit-install-language-grammar' before editing.
  (setq major-mode-remap-alist
        '((yaml-mode . yaml-ts-mode)
          (bash-mode . bash-ts-mode)
          (js2-mode . js-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (json-mode . json-ts-mode)
          (css-mode . css-ts-mode)
          (python-mode . python-ts-mode))))

(use-package project
  :custom (when (>= emacs-major-version 30)
            (project-mode-line t)))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package markdown-mode
  :ensure t
  :hook (markdown-mode . visual-line-mode))

(use-package yaml-mode
  :ensure t)

(use-package json-mode
  :ensure t)

(use-package makefile
  :ensure nil
  :hook (makefile-mode . (lambda () (setq indent-tabs-mode t))))

(use-package eglot
  :custom
  (eglot-send-changes-idle-time 0.1)
  (eglot-extend-to-xref t)
  :config (fset #'jsonrpc--log-event #'ignore))

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
  :hook
  ((prog-mode . tempel-setup-capf)
   (text-mode . tempel-setup-capf))
  :init
  ;; Make a function that adds the tempel expansion function to the
  ;; list of completion-at-point-functions (capf).
  (defun tempel-setup-capf ()
    (add-hook 'completion-at-point-functions #'tempel-expand -1 'local)))

(use-package super-save
  :ensure t
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t))

(use-package inheritenv
  :vc (:url "https://github.com/purcell/inheritenv" :rev :newest))

(use-package claude-code
  :ensure t
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
  :config (claude-code-mode)
  :bind-keymap ("C-c c" . claude-code-command-map)
  :bind (:repeat-map sz/claude-code-map ("M" . claude-code-cycle-mode)))

(use-package elfeed
  :ensure t)

(use-package elfeed-org
  :ensure t
  :config
  (setq rmh-elfeed-org-files (list "~/org/rss.org"))
  (elfeed-org))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package nix-mode
  :ensure t
  :mode "\\.nix^\\'"
  :hook (before-save . nix-format-before-save))

(use-package windmove
  :ensure nil
  :bind* (("M-h" . windmove-left)
          ("M-l" . windmove-right)
          ("M-j" . windmove-down)
          ("M-k" . windmove-up)))

(use-package agent-shell
  :ensure t)
