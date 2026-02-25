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
       (base (expand-file-name "sz-base.el" dir))
       (base-true (and (file-exists-p base) (file-truename base))))
  (when base-true
    (load base nil 'nomessage))
  (dolist (file (directory-files dir t "\\.el\\'"))
    (let ((name (file-name-nondirectory file)))
      (unless (or (and base-true (string= (file-truename file) base-true))
                  (string-prefix-p ".#" name)
                  (string-prefix-p "#" name)
                  (string-suffix-p "~" name))
        (load file nil 'nomessage)))))
