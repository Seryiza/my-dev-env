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
(load custom-file nil t)

(add-to-list 'load-path (locate-user-emacs-file "lisp"))

(let* ((dir (locate-user-emacs-file "lisp"))
       (functions-dir (expand-file-name "functions" dir))
       (files '("sz-base.el"
                "sz-theme.el"
                "sz-meow.el"
                "sz-eat.el"
                "sz-alerts.el"
                "sz-org.el"
                "sz-telega.el"
                "sz-mode-line.el")))

  ;; Load my emacs modules
  (dolist (file files)
    (load (expand-file-name file dir) nil 'nomessage))

  ;; Load elisp commands and functions
  (when (file-directory-p functions-dir)
    (dolist (file (directory-files functions-dir t "\\.el\\'"))
      (load file nil 'nomessage))))
