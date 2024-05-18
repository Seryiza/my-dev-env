;;; Emacs Bedrock
;;;
;;; Extra config: Development tools

;;; Usage: Append or require this file from init.el for some software
;;; development-focused packages.
;;;
;;; It is **STRONGLY** recommended that you use the base.el config if you want to
;;; use Eglot. Lots of completion things will work better.
;;;
;;; This will try to use tree-sitter modes for many languages. Please run
;;;
;;;   M-x treesit-install-language-grammar
;;;
;;; Before trying to use a treesit mode.

;;; Contents:
;;;
;;;  - Built-in config for developers
;;;  - Version Control
;;;  - Common file types
;;;  - Eglot, the built-in LSP client for Emacs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Built-in config for developers
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package emacs
  :config
  ;; Treesitter config

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
  ;; Auto parenthesis matching
  ((prog-mode . electric-pair-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Version Control
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Magit: best Git client to ever exist
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Common file types
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package markdown-mode
  :hook ((markdown-mode . visual-line-mode)))

(use-package yaml-mode
  :ensure t)

(use-package json-mode
  :ensure t)

;; Emacs ships with a lot of popular programming language modes. If it's not
;; built in, you're almost certain to find a mode for the language you're
;; looking for with a quick Internet search.

;; Clojure tooling

(use-package clojure-mode
  :ensure t)

;(use-package clojure-ts-mode
;  :ensure t)

(use-package cider
  :ensure t)

(use-package lua-mode
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Eglot, the built-in LSP client for Emacs
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Helpful resources:
;;
;;  - https://www.masteringemacs.org/article/seamlessly-merge-multiple-documentation-sources-eldoc

(use-package eglot
  ;; no :ensure t here because it's built-in

  ;; Configure hooks to automatically turn-on eglot for selected modes
  :hook
  (((clojure-mode clojure-ts-mode clojurec-ts-mode clojurescript-ts-mode lua-mode) . eglot))

  :custom
  (eglot-send-changes-idle-time 0.1)
  (eglot-extend-to-xref t)              ; activate Eglot in referenced non-project files

  :config
  (fset #'jsonrpc--log-event #'ignore)  ; massive perf boost---don't log every event
  ;; Sometimes you need to tell Eglot where to find the language server
  (add-to-list 'eglot-server-programs
               '(clojure-mode . ("clojure-lsp")))
  )

;(use-package lsp-mode
;  :ensure t
;  :init
;  (projectile-mode +1)
;
;  (defun my/lsp-mode-setup-completion ()
;    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
;          '(orderless))) ;; Configure orderless

;    :hook
;    ((clojure-mode . lsp)
;     (clojurec-mode . lsp)
;     (clojurescript-mode . lsp)
;     (lua-mode . lsp)
;     (lsp-completion-mode . my/lsp-mode-setup-completion));

 ;   :custom
 ;   (lsp-completion-provider :none)

  ;  :config
    ;; add paths to your local installation of project mgmt tools, like lein
 ;   (setq lsp-enable-symbol-highlighting nil)
 ;   (setq lsp-clients-lua-language-server-command "lua-language-server")
 ;   (setq lsp-clients-lua-language-server-bin (executable-find "lua-language-server"))
 ;   (setq read-process-output-max (* 1024 1024))
  ;  (setenv "PATH" (concat
;		    "/usr/local/bin" path-separator
 ;                   (getenv "PATH")))
;;    (dolist (m '(clojure-mode
 ;;                clojurec-mode
 ;;                clojurescript-mode
  ;;               clojurex-mode))
    ;;  (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))
  ;  )

(use-package rg
  :ensure t)

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)

  :bind (:map projectile-mode-map
              ("M-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

(use-package helm
  :ensure t
  :config (require 'helm-autoloads))

(use-package helm-rg
  :ensure t)

(use-package helm-lsp
  :ensure t)

(use-package helm-projectile
  :ensure t)
