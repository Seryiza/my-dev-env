;; Bootstrap package system

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(require 'use-package)

(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file nil t))

(add-to-list 'load-path (locate-user-emacs-file "lisp"))

(let* ((dir (locate-user-emacs-file "lisp"))
       (base (expand-file-name "sz-base.el" dir)))
  (when (file-exists-p base)
    (load base nil 'nomessage))
  (dolist (file (directory-files dir t "\\.el\\'"))
    (unless (string= (file-truename file) (file-truename base))
      (load file nil 'nomessage))))
