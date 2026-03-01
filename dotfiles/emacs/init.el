;; Bootstrap package system

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(require 'use-package)

;; Packages installed in NixOS
(setq package-hidden-regexps
      '("\\`vterm\\'"
        "\\`telega\\'"
        "\\`visual-fill-column\\'"
        "\\`mu4e\\'"))

(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file nil t))

(add-to-list 'load-path (locate-user-emacs-file "lisp"))

(let* ((dir (locate-user-emacs-file "lisp"))
       (files '("sz-base.el"
                "sz-meow.el"
                "sz-alerts.el"
                "sz-org.el"
                "sz-telega.el"
                "sz-external-functions.el")))
  (dolist (file files)
    (load (expand-file-name file dir) nil 'nomessage)))
