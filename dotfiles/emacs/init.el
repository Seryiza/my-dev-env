;;; $$$$$$$$/ _____  ____   ______   _______  _______       $$$$$$$  | ______   ____$$ | ______   ______   _______$$ |   __
;;; $$ |__   /     \/    \ /      \ /       |/       |      $$ |__$$ |/      \ /    $$ |/      \ /      \ /       $$ |  /  |
;;; $$    |  $$$$$$ $$$$  |$$$$$$  /$$$$$$$//$$$$$$$/       $$    $$</$$$$$$  /$$$$$$$ /$$$$$$  /$$$$$$  /$$$$$$$/$$ |_/$$/
;;; $$$$$/   $$ | $$ | $$ |/    $$ $$ |     $$      \       $$$$$$$  $$    $$ $$ |  $$ $$ |  $$/$$ |  $$ $$ |     $$   $$<
;;; $$ |_____$$ | $$ | $$ /$$$$$$$ $$ \_____ $$$$$$  |      $$ |__$$ $$$$$$$$/$$ \__$$ $$ |     $$ \__$$ $$ \_____$$$$$$  \
;;; $$       $$ | $$ | $$ $$    $$ $$       /     $$/       $$    $$/$$       $$    $$ $$ |     $$    $$/$$       $$ | $$  |
;;; $$$$$$$$/$$/  $$/  $$/ $$$$$$$/ $$$$$$$/$$$$$$$/        $$$$$$$/  $$$$$$$/ $$$$$$$/$$/       $$$$$$/  $$$$$$$/$$/   $$/

;;; Minimal init.el

;;; Contents:
;;;
;;;  - Basic settings
;;;  - Discovery aids
;;;  - Minibuffer/completion settings
;;;  - Interface enhancements/defaults
;;;  - Tab-bar configuration
;;;  - Theme
;;;  - Optional extras
;;;  - Built-in customization framework

;;; Guardrail

(when (< emacs-major-version 29)
  (error (format "Emacs Bedrock only works with Emacs 29 and newer; you have version ~a" emacs-major-version)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Basic settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Package initialization
;;
;; We'll stick to the built-in GNU and non-GNU ELPAs (Emacs Lisp Package
;; Archive) for the base install, but there are some other ELPAs you could look
;; at if you want more packages. MELPA in particular is very popular. See
;; instructions at:
;;
;;    https://melpa.org/#/getting-started
;;
;; You can simply uncomment the following if you'd like to get started with
;; MELPA packages quickly:
;;
(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

(customize-set-variable 'package-archive-priorities
			  '(("gnu"    . 30)
                ("nongnu" . 40)
                ("melpa"  . 50)))

(use-package quelpa
  :ensure t)

(use-package quelpa-use-package
  :ensure t)

;; If you want to turn off the welcome screen, uncomment this
;(setopt inhibit-splash-screen t)

(setopt initial-major-mode 'fundamental-mode)  ; default mode for the *scratch* buffer
(setopt display-time-default-load-average nil) ; this information is useless for most

;; Automatically reread from disk if the underlying file changes
(setopt auto-revert-avoid-polling t)
;; Some systems don't do file notifications well; see
;; https://todo.sr.ht/~ashton314/emacs-bedrock/11
(setopt auto-revert-interval 5)
(setopt auto-revert-check-vc-info t)
(global-auto-revert-mode)

;;  history of minibuffer
(savehist-mode)

;; Move through windows with Ctrl-<arrow keys>
;; (windmove-default-keybindings 'control) ; You can use other modifiers here

;; (setq window-combination-resize t
;;       split-width-threshold 100)

;; Fix archaic defaults
(setopt sentence-end-double-space nil)

;; Make right-click do something sensible
(when (display-graphic-p)
  (context-menu-mode))

(setq dired-listing-switches "-al  --group-directories-first")

;; Don't litter file system with *~ backup files; put them all inside
;; ~/.emacs.d/backup or wherever
(defun bedrock--backup-file-name (fpath)
  "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
  (let* ((backupRootDir "~/.emacs.d/emacs-backup/")
         (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path
         (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") )))
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath))

(setopt make-backup-file-name-function 'bedrock--backup-file-name)

;(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))

(setq create-lockfiles nil)

(setq tab-width 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Discovery aids
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Show the help buffer after startup
					;(add-hook 'after-init-hook 'help-quick)

;; which-key: shows a popup of available keybindings when typing a long key
;; sequence (e.g. C-x ...)
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Minibuffer/completion settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For help, see: https://www.masteringemacs.org/article/understanding-minibuffer-completion

(setopt enable-recursive-minibuffers t)                ; Use the minibuffer whilst in the minibuffer
(setopt completion-cycle-threshold 1)                  ; TAB cycles candidates
(setopt completions-detailed t)                        ; Show annotations
(setopt tab-always-indent 'complete)                   ; When I hit TAB, try to complete, otherwise, indent
(setopt completion-styles '(basic initials substring)) ; Different styles to match input to candidates

(setopt completion-auto-help 'always)                  ; Open completion always; `lazy' another option
(setopt completions-max-height 20)                     ; This is arbitrary
(setopt completions-detailed t)
(setopt completions-format 'one-column)
(setopt completions-group t)
(setopt completion-auto-select 'second-tab)            ; Much more eager
;(setopt completion-auto-select t)                     ; See `C-h v completion-auto-select' for more possible values

(keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete) ; TAB acts more like how it does in the shell

;; For a fancier built-in completion option, try ido-mode,
;; icomplete-vertical, or fido-mode. See also the file extras/base.el

;(icomplete-vertical-mode)
;(fido-vertical-mode)
;(setopt icomplete-delay-completions-threshold 4000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Interface enhancements/defaults
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Mode line information
(setopt line-number-mode t)                        ; Show current line in modeline
(setopt column-number-mode t)                      ; Show column as well

(setopt x-underline-at-descent-line nil)           ; Prettier underlines
(setopt switch-to-buffer-obey-display-actions t)   ; Make switching buffers more consistent

(setopt show-trailing-whitespace nil)      ; By default, don't underline trailing spaces
(setopt indicate-buffer-boundaries 'left)  ; Show buffer top and bottom in the margin

;; Enable horizontal scrolling
(setopt mouse-wheel-tilt-scroll t)
(setopt mouse-wheel-flip-direction t)

;; We won't set these, but they're good to know about
;;
;; (setopt indent-tabs-mode nil)
;; (setopt tab-width 4)

;; Misc. UI tweaks
(blink-cursor-mode -1)                                ; Steady cursor
(pixel-scroll-precision-mode)                         ; Smooth scrolling

;; TODO: check org-timeblock with scrollbars and 1:1
(scroll-bar-mode -1)

;; Use common keystrokes by default
(cua-mode)

(define-key cua-global-keymap [C-return] nil)

;; Display line numbers in programming mode
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'org-mode-hook 'display-line-numbers-mode)
(setopt display-line-numbers-width 3)           ; Set a minimum width

;; Nice line wrapping when working with text
(add-hook 'text-mode-hook 'visual-line-mode)

;; Modes to highlight the current line with
(let ((hl-line-hooks '(text-mode-hook prog-mode-hook)))
  (mapc (lambda (hook) (add-hook hook 'hl-line-mode)) hl-line-hooks))

;; Disable the small delay before parenthesis highlighting
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Hide menu bar
(menu-bar-mode -1)

;; Hide tool bar
;(tool-bar-mode -1)

;; Hide fringe bar
(set-fringe-mode 0)

;; TODO: think about bell sound / flash
;; https://www.emacswiki.org/emacs/AlarmBell
(setq ring-bell-function 'ignore)

;; Enable autopair

;; (electric-pair-mode t)
;; (setq electric-pair-preserve-balance nil)

;; (use-package smartparens
;;   :ensure t
;;   :hook (prog-mode text-mode markdown-mode) ;; add `smartparens-mode` to these hooks
;;   :config
;;   ;; load default config
;;   (require 'smartparens-config))

;; Paredit

(use-package paredit
  :ensure t
  :hook
  (emacs-lisp-mode . paredit-mode)
  (clojure-mode . paredit-mode))

;; TODO: move font name to variable / constant
(add-to-list 'default-frame-alist '(font . "Iosevka-15" ))
(set-face-attribute 'default t :font "Iosevka-15" )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Tab-bar configuration
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Show the tab-bar as soon as tab-bar functions are invoked
;; (setopt tab-bar-show 1)

;; Add the time to the tab-bar, if visible
;; (add-to-list 'tab-bar-format 'tab-bar-format-align-right 'append)
;; (add-to-list 'tab-bar-format 'tab-bar-format-global 'append)
;; (setopt display-time-format "%a %F %T")
;; (setopt display-time-interval 1)
;; (display-time-mode)

;; My packages

;(use-package catppuccin-theme
;  :demand t
;  :ensure t
;  :config
;  (setq catppuccin-flavor 'latte)
;  (load-theme 'catppuccin :no-confirm)
;  (catppuccin-reload))

(use-package nerd-icons
  ;; :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  ;; (nerd-icons-font-family "Symbols Nerd Font Mono")
  )

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold nil   ; if nil, bold is universally disabled
        doom-themes-enable-italic nil) ; if nil, italics is universally disabled
  ;(load-theme 'doom-bluloco-light t)
  ;(load-theme 'doom-one-light t)
  ;(load-theme 'doom-solarized-light t)

  ;; Enable flashing mode-line on errors
  ;(doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;(doom-themes-neotree-config)
  ;; or for treemacs users
  ;(setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  ;(doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  ;(doom-themes-org-config)
  )

;; (use-package stimmung-themes
;;   :demand t
;;   :ensure t
;;   :config (stimmung-themes-load-light)) ; or (stimmung-themes-load-dark)

;; (use-package nano-theme
;;   :quelpa (nano-theme
;;            :fetcher github
;;            :repo "rougier/nano-theme")
;;   :config
;;   (nano-light)

;;   ;; TODO: use custom-theme-set-faces
;;   ;(with-eval-after-load "nano-theme"
;;   ;  (custom-theme-set-faces
;;   ;   'nano
;;   ;   ;; TODO: fix weight only for TODO items
;;   ;   '(org-level-2 ((t (:weight normal))))
;;   ;   '(org-level-3 ((t (:weight normal))))
;;   ;   '(org-level-4 ((t (:weight normal))))
;;   ;   '(org-level-5 ((t (:weight normal))))))

;;   :custom-face
;;   (vertical-border ((t (:foreground "black"))))

;;   )

;; (use-package nano-modeline
;;   :ensure t

;;   :config
;;   (setq nano-modeline-position 'nano-modeline-footer)  
;;   ;; (setq nano-modeline-position 'nano-modeline-header)

;;   (add-hook 'prog-mode-hook #'nano-modeline-prog-mode)
;;   (add-hook 'text-mode-hook #'nano-modeline-text-mode)
;;   (add-hook 'org-mode-hook #'nano-modeline-org-mode)

;;   (nano-modeline-text-mode t))

					;(use-package doom-modeline
					;  :ensure t
					;  :init (doom-modeline-mode 1))

;; Treesitter

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
	(css "https://github.com/tree-sitter/tree-sitter-css")
	(clojure "https://github.com/sogaiu/tree-sitter-clojure")
	(elisp "https://github.com/Wilfred/tree-sitter-elisp")
	(html "https://github.com/tree-sitter/tree-sitter-html")
	(javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
	(json "https://github.com/tree-sitter/tree-sitter-json")
	(make "https://github.com/alemuller/tree-sitter-make")
	(markdown "https://github.com/ikatyang/tree-sitter-markdown")
	(yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Theme
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package emacs
 :config
 (setq shr-use-fonts nil)
 (load-theme 'modus-operandi))

(use-package doom-modeline
  :ensure t

  :config
  (setq doom-modeline-icon nil)
  
  :init
  (doom-modeline-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Optional extras
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Uncomment the (load-file …) lines or copy code from the extras/ elisp files
;; as desired

;; UI/UX enhancements mostly focused on minibuffer and autocompletion interfaces
;; These ones are *strongly* recommended!
(load-file (expand-file-name "extras/base.el" user-emacs-directory))

;; Packages for software development
(load-file (expand-file-name "extras/dev.el" user-emacs-directory))

;; Meow
(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k")
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

(defun +translate-input-event (event)
  "Use reverse-im to translate input EVENT."
  (if (numberp event)
      (reverse-im--translate-char event t)
    event))

(use-package meow
  :ensure t

  :config
  (meow-setup)

  (setq meow-paren-keymap (make-keymap))

  (meow-define-state paren
    "meow state for interacting with paredit"
    :lighter " [P]"
    :keymap meow-paren-keymap)

  ;; meow-define-state creates the variable
  (setq meow-cursor-type-paren 'hollow)

  (meow-define-keys 'paren
  '("<escape>" . meow-normal-mode)
  '("i" . meow-insert-mode)
  '("l" . paredit-forward)
  '("h" . paredit-backward)
  '("j" . paredit-forward-down)
  '("k" . paredit-backward-up)
  '("n" . paredit-forward-slurp-sexp)
  '("b" . paredit-forward-barf-sexp)
  '("v" . paredit-backward-barf-sexp)
  '("c" . paredit-backward-slurp-sexp)
  '("u" . meow-undo))

  (setq meow-expand-hint-counts '((word . 0)
                                  (line . 30)
                                  (block . 30)
                                  (find . 0)
                                  (till . 30)))

  (setq meow-selection-command-fallback
	'((meow-change . meow-change-char)
	  ;(meow-kill . meow-C-k)
	  (meow-cancel-selection . keyboard-quit)
	  (meow-pop-selection . meow-pop-grab)
	  (meow-beacon-change . meow-beacon-change-char)))

  (when window-system
    (define-key input-decode-map (kbd "C-[") [control-bracketleft])
    ;; NOTE: it's a russian "х"
    (define-key input-decode-map (kbd "C-х") [control-bracketleft])
    (define-key meow-insert-state-keymap [control-bracketleft] 'meow-insert-exit))

  (add-hook 'meow-insert-exit-hook 'corfu-quit)

  ;; TODO: refactor normal & motion shared binds
  (meow-define-keys
      'normal

    '("C-;" . meow-comment)
    '("C-q" . delete-other-windows)

    ;; org-mode items
    '("C-h" . org-promote-subtree)
    '("C-j" . org-move-subtree-down)
    '("C-k" . org-move-subtree-up)
    '("C-l" . org-demote-subtree)

    '("M-h" . windmove-left)
    '("M-j" . windmove-down)
    '("M-k" . windmove-up)
    '("M-l" . windmove-right)

    '("M-n" . next-buffer)
    '("M-m" . previous-buffer))
  
  (meow-define-keys
      'motion

    '("h" . meow-left)
    '("l" . meow-right)

    '("C-q" . delete-other-windows)

    '("M-h" . windmove-left)
    '("M-j" . windmove-down)
    '("M-k" . windmove-up)
    '("M-l" . windmove-right)

	'("M-n" . next-buffer)
    '("M-m" . previous-buffer))

  (meow-leader-define-key
   'normal
   '("s" . save-buffer))

  (setq meow-visit-sanitize-completion nil)
  (setq meow-use-clipboard t)

  ;; workaround to fix meow + reverse-im
  ;; see https://github.com/meow-edit/meow/discussions/211
  (advice-add 'meow--event-key :filter-return #'+translate-input-event)
  (meow-global-mode 1))

;; Vim-bindings in Emacs (evil-mode configuration)
;; (load-file (expand-file-name "extras/vim-like.el" user-emacs-directory))

;; Org-mode configuration
;; WARNING: need to customize things inside the elisp file before use! See
;; the file extras/org-intro.txt for help.
(load-file (expand-file-name "extras/org.el" user-emacs-directory))

;; Email configuration in Emacs
;; WARNING: needs the `mu' program installed; see the elisp file for more
;; details.
;(load-file (expand-file-name "extras/email.el" user-emacs-directory))

;; Tools for academic researchers
;(load-file (expand-file-name "extras/researcher.el" user-emacs-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Built-in customization framework
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-visited-interval 5)
 '(auto-save-visited-mode t)
 '(global-eldoc-mode nil)
 '(package-selected-packages
   '(elfeed-org elfeed zprint-mode yaml-mode which-key vterm vertico stimmung-themes smartparens rg reverse-im quelpa-use-package popup paredit org-timeblock org-roam orderless nix-mode nano-theme nano-modeline meow marginalia magit lua-mode kind-icon json-mode howm helm-rg helm-projectile helm-lsp evil embark-consult eglot-booster doom-themes doom-modeline corfu-terminal clojure-ts-mode cider catppuccin-theme cape avy))
 '(wgrep-auto-save-buffer t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; My common keybindings

(global-set-key (kbd "C-c b r") 'rename-buffer)
(global-set-key (kbd "M-M") 'xref-find-references)

;; My functions

(defun my-edit-configuration ()
  "Open the init file."
  (interactive)
  (find-file user-init-file))

(defun my-eval-configuration ()
  "Eval the init file."
  (interactive)
  (save-buffer)
  (eval-expression (load-file user-init-file)))

(use-package elfeed
  :ensure t)

(use-package elfeed-org
  :ensure t
  :config
  (setq rmh-elfeed-org-files (list "~/nazarick/rss.org"))
  (elfeed-org))
