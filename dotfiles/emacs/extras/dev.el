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
  :bind
  ("M-." . xref-find-definitions)
  
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
          (python-mode . python-ts-mode))))

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

(use-package flycheck
  :ensure t

  :config
  (global-flycheck-mode))

(use-package flycheck-clj-kondo
  :ensure t)

(use-package clojure-mode
  :ensure t

  :config
  ;; (setq clojure-indent-style 'always-indent
  ;;       clojure-indent-keyword-style 'always-indent
  ;;       clojure-enable-indent-specs nil)

  (require 'flycheck-clj-kondo))

;(use-package clojure-ts-mode
;  :ensure t)

;; (use-package zprint-mode
;;   :ensure t
;;   :hook ((clojure-mode . zprint-mode)
;; 		 (clojurec-mode . zprint-mode)
;; 	 (clojurescript-mode . zprint-mode)))

(defun format-clojure ()
  (when (or (eq major-mode 'clojure-mode)
	        (eq major-mode 'clojurec-mode)
            (eq major-mode 'clojurescript-mode))
    (shell-command-to-string
     (if (file-exists-p (concat (projectile-project-root buffer-file-name) ".cljfmt.edn"))
	 (format "cljfmt fix --config %s %s"
      			      (concat (projectile-project-root buffer-file-name) ".cljfmt.edn")
					  buffer-file-name)
       (format "cljfmt fix %s" buffer-file-name)))
    (revert-buffer :ignore-auto :noconfirm)))

(use-package cider
  :ensure t

  :bind
  (:map clojure-mode-map
	("C-c e C-e" . cider-eval-list-at-point)
	("C-c e C-b" . cider-eval-buffer)
	("C-c e C-f" . cider-eval-defun-at-point)
	("C-c t C-c" . cider-test-run-test)
	("C-c t C-n" . cider-test-run-ns-tests)
	("C-c t C-r" . cider-test-show-report))

  :config
  (setq cider-save-file-on-load t)
  (setq cider-use-xref nil)
  (setq cider-font-lock-reader-conditionals nil)

  ;; (setq cider-format-code-options
  ;;     '(("sort-ns-references?" (("true")))))
  
  (add-hook 'after-save-hook #'format-clojure
			;t
			;t
			))

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
  ;; :hook
  ;; (((clojure-mode clojurec-mode clojurescript-mode lua-mode) . eglot-ensure))

  :custom
  (eglot-send-changes-idle-time 0.1)
  (eglot-extend-to-xref t)              ; activate Eglot in referenced non-project files

  :config
  (fset #'jsonrpc--log-event #'ignore)  ; massive perf boost---don't log every event
  ;; Sometimes you need to tell Eglot where to find the language server
  ;(add-to-list 'eglot-server-programs
  ;             '(clojure-mode . ("clojure-lsp")))
  (setq eglot-connect-timeout 600)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  (add-hook 'eglot-managed-mode-hook (lambda () (eldoc-mode -1)))
  )

(use-package eglot-booster
  :ensure t
  :quelpa (eglot-booster
            :fetcher github
            :repo "jdtsmith/eglot-booster")
  :after eglot
  :config (eglot-booster-mode))

					;(Use-package lsp-mode
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

  :bind
  ("C-c o C-s" . projectile-run-shell)
  ("C-c o C-v" . projectile-run-vterm)
  ("C-c f C-f" . projectile-find-file))

(use-package makefile-executor
  :ensure t
  :config
  (add-hook 'makefile-mode-hook 'makefile-executor-mode))

(use-package helm
  :ensure t

  ;; :bind
  ;; ("C-c f C-g" . helm-projectile-ag)
  ;; ("C-c f C-l" . helm-resume)

  :config
  (require 'helm-autoloads))

(use-package helm-rg
  :ensure t)

(use-package helm-ag
  :ensure t)

(use-package helm-lsp
  :ensure t)

(use-package helm-projectile
  :ensure t)

;;; Emacs Lisp


;; TODO: fix eval-sexp-at-point with (insert "Hello!") inside quotes
(defun my-elisp-eval-sexp-at-point ()
  "Eval the smallest surrounding sexp at point."
  (interactive)
  (save-excursion
    ;; Move to the start of the surrounding list/sexp, then forward over it,
    ;; so `eval-last-sexp` evaluates that sexp.
    (condition-case _
        (progn
          (backward-up-list 1)
          (forward-sexp 1)
          (eval-last-sexp nil))
      (error
       ;; If not inside a list, try just eval the preceding sexp.
       (eval-last-sexp nil)))))

(use-package emacs
  :ensure nil
  :bind
  (:map emacs-lisp-mode-map
		("C-c e C-e" . my-elisp-eval-sexp-at-point)
		("C-c e C-b" . eval-buffer)
		("C-c e C-f" . eval-defun)))

(use-package eat
  :ensure t
  
  :after meow

  :hook
  (eat-mode . eat-emacs-mode)
  (eat-mode . meomacs-eat-meow-setup)

  :commands
  (eat eat-emacs-mode eat-char-mode eat-semi-char-mode eat-yank eat-self-input)

  :init
  (defun meomacs-eat-meow-setup ()
    "Integrate Eat with Meow per-buffer."
    (add-hook 'meow-normal-mode-hook #'eat-emacs-mode nil t)
    (add-hook 'meow-insert-mode-hook #'eat-char-mode  nil t))

  :config
  (define-key eat-char-mode-map (kbd "C-y") #'eat-yank)
  (advice-add #'eat-semi-char-mode :after #'eat-emacs-mode))

(use-package inheritenv
  :vc (:url "https://github.com/purcell/inheritenv" :rev :newest))

(use-package claude-code
  :ensure t
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
  
  :config
  ;; optional IDE integration with Monet
  ;; (add-hook 'claude-code-process-environment-functions #'monet-start-server-function)
  ;; (monet-mode 1)

  (setq claude-code-terminal-backend 'eat)
  (claude-code-mode)
  
  ;; :bind-keymap
  ;; ("C-c a" . claude-code-command-map)

  ;; Optionally define a repeat map so that "M" will cycle thru Claude auto-accept/plan/confirm modes after invoking claude-code-cycle-mode / C-c M.
  :bind
  (:repeat-map my-claude-code-map ("M" . claude-code-cycle-mode)))
