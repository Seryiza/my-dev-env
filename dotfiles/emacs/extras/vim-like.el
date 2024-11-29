;;; Emacs Bedrock
;;;
;;; Extra config: Vim emulation

;;; Usage: Append or require this file from init.el for bindings in Emacs.

;;; Contents:
;;;
;;;  - Core Packages

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Core Packages
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Evil: vi emulation
(use-package evil
  :ensure t

  :init
  (setq evil-respect-visual-line-mode t)
  (setq evil-undo-system 'undo-redo)

  ;; Enable this if you want C-u to scroll up, more like pure Vim
  (setq evil-want-C-u-scroll t)

  :config
  (evil-mode)

  ;; Configuring initial major mode for some modes
  (evil-set-initial-state 'vterm-mode 'emacs)

  ;; Set global leader
  (evil-set-leader (list 'normal 'visual 'motion) (kbd "SPC"))

  ;; Set local leader
  ;(evil-set-leader nil "," t)

  ;; Examples
  ;(evil-global-set-key 'normal (kbd "<leader>fs") 'save-buffer)

  ;; My defines
  (define-key evil-motion-state-map (kbd "RET") nil)
  (evil-global-set-key 'normal (kbd "<leader>qi") 'my-edit-configuration)
  (evil-global-set-key 'normal (kbd "<leader>qu") 'my-eval-configuration)
  (evil-global-set-key 'normal (kbd "<leader>m") 'execute-extended-command)
  (evil-global-set-key 'normal (kbd "<leader>c") 'comment-line)
  (evil-global-set-key 'normal (kbd "<leader>bb") 'consult-buffer)
  (evil-global-set-key 'normal (kbd "<leader>br") 'rename-buffer)
  (evil-global-set-key 'normal (kbd "<leader>os") 'projectile-run-shell)
;
  (evil-global-set-key 'normal (kbd "<leader>ff") 'projectile-find-file)
  (evil-global-set-key 'normal (kbd "<leader>fg") 'helm-projectile-rg)
  (evil-global-set-key 'normal (kbd "<leader>fl") 'helm-resume)

  ;; cider
  (evil-global-set-key 'normal (kbd "<leader>ee") 'cider-eval-list-at-point)
  (evil-global-set-key 'normal (kbd "<leader>eb") 'cider-eval-buffer)
  (evil-global-set-key 'normal (kbd "<leader>ef") 'cider-eval-defun-at-point)

  (evil-global-set-key 'normal (kbd "<leader>tc") 'cider-test-run-test)
  (evil-global-set-key 'normal (kbd "<leader>tn") 'cider-test-run-ns-tests)
  (evil-global-set-key 'normal (kbd "<leader>tr") 'cider-test-show-report)

  ;; windows management
  (evil-global-set-key 'normal (kbd "M-h") 'evil-window-left)
  (evil-global-set-key 'normal (kbd "M-j") 'evil-window-down)
  (evil-global-set-key 'normal (kbd "M-k") 'evil-window-up)
  (evil-global-set-key 'normal (kbd "M-l") 'evil-window-right))
