;;; Emacs Bedrock
;;;
;;; Extra config: Org-mode starter config

;;; Usage: Append or require this file from init.el for some software
;;; development-focused packages.
;;;
;;; Org-mode is a fantastically powerful package. It does a lot of things, which
;;; makes it a little difficult to understand at first.
;;;
;;; We will configure Org-mode in phases. Work with each phase as you are
;;; comfortable.
;;;
;;; YOU NEED TO CONFIGURE SOME VARIABLES! The most important variable is the
;;; `org-directory', which tells org-mode where to look to find your agenda
;;; files.

;;; See "org-intro.txt" for a high-level overview.

;;; Contents:
;;;
;;;  - Critical variables
;;;  - Phase 1: editing and exporting files
;;;  - Phase 2: todos, agenda generation, and task tracking
;;;  - Phase 3: extensions (org-roam, etc.)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Critical variables
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; These variables need to be set for Org-mode's full power to be unlocked!
;;;
;;; You can read the documentation for any variable with `C-h v'. If you have
;;; Consult configured (see the `base.el' file) then it should help you find
;;; what you're looking for.

;;; Phase 1 variables

;;; Phase 2 variables

;; Agenda variables
(setq org-directory "~/org") ; Non-absolute paths for agenda and
                                 ; capture templates will look here.

; (setq org-agenda-files '("~/org"))

;(setq org-agenda-files (append
;                        '("~/orglife")
;                        (directory-files-recursively "~/orglife/howm" "\\.org\\'")))

(setq org-agenda-files (directory-files-recursively "~/org" "\\.org\\'"))

;; Default tags
(setq org-tag-alist '(
                      ;; locale
                      (:startgroup)
                      ("home" . ?h)
                      ("work" . ?w)
                      ("school" . ?s)
                      (:endgroup)
                      (:newline)
                      ;; scale
                      (:startgroup)
                      ("one-shot" . ?o)
                      ("project" . ?j)
                      ("tiny" . ?t)
                      (:endgroup)
                      ;; misc
                      ("meta")
                      ("review")
                      ("reading")))

;; Org-refile: where should org-refile look?
(setq org-refile-targets 'FIXME)

;;; Phase 3 variables

;; Org-roam variables
(setq org-roam-directory "~/orglife/roam")
(setq org-roam-index-file "~/orglife/roam/index.org")

;;; Optional variables

;; Advanced: Custom link types
;; This example is for linking a person's 7-character ID to their page on the
;; free genealogy website Family Search.
(setq org-link-abbrev-alist
      '(("family_search" . "https://www.familysearch.org/tree/person/details/%s")))

(defadvice org-agenda (around split-vertically activate)
  (let ((split-width-threshold 80))  ; or whatever width makes sense for you
    ad-do-it))

(defun my-today-agenda ()
  (interactive)
  (org-agenda nil "n"))

(defun my-work-agenda ()
  (interactive)
  (org-agenda nil "w"))

(defun todo-agenda ()
  (interactive)
  (org-agenda nil "t"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Phase 1: editing and exporting files
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org
  ;; :hook ((org-mode . visual-line-mode)  ; wrap lines at word breaks
  ;; 					(org-mode . flyspell-mode)
  ;; 	 )    ; spell checking!

  ;; :bind (:map global-map
  ;;             ;("C-c l s" . org-store-link)          ; Mnemonic: link → store
  ;;             ;("C-c l i" . org-insert-link-global) ; Mnemonic: link → insert
  ;; 	      )

  :bind
  (("C-c . C-a" . org-agenda-list)
   ("C-c . ." . org-capture)
   ("C-c . C-j" . my-today-agenda)
   ("C-c . C-w" . my-work-agenda)
   ("C-c . C-t" . todo-agenda))
  
  :config
					;(require 'oc-csl)                     ; citation support
  (add-to-list 'org-export-backends 'md)

  ;; Make org-open-at-point follow file links in the same window
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)

  ;; Make exporting quotes better
  (setq org-export-with-smart-quotes t)

  (setq org-agenda-prefix-format
	'(			     ;(agenda . " %i %-12:c%?-12t% s")
					;(agenda . "  %?-12t% s")
	  (agenda . " %?-12t% s")
					;(todo . " %i %-12:c")
	  (todo . " %i")
	  (tags . " %i %-12:c")
	  (search . " %i %-12:c")))

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Phase 2: todos, agenda generation, and task tracking
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Yes, you can have multiple use-package declarations. It's best if their
;; configs don't overlap. Once you've reached Phase 2, I'd recommend merging the
;; config from Phase 1. I've broken it up here for the sake of clarity.
(use-package org
  :config
  ;; Instead of just two states (TODO, DONE) we set up a few different states
  ;; that a task can be in.
  (setq org-todo-keywords
        '((sequence "TODO(t)" "STARTED(s!)" "WAITING(w!)" "NEXT(n)" "MEETING(e)" "TIMEBLOCK(b)" "|" "DONE(d)" "CANCELED(c)")))

  ;; Refile configuration
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-use-outline-path 'file)
  (setq org-link-descriptive nil)
  ;; TODO: improve it
  (setq org-M-RET-may-split-line '((default . nil)))
  ;; (setq org-M-RET-may-split-line '((item . t)))
  ;; TODO: change formatting for LOGBOOK entries
  (setq org-log-into-drawer "LOGBOOK")
  (setq org-log-repeat 'time)
  (setq org-log-done 'time)
  (setq org-agenda-window-setup 'current-window)
  ;(setq org-agenda-include-inactive-timestamps t)
  ;(setq org-agenda-log-mode-items '(closed clock state))
  ;(setq org-agenda-start-with-log-mode '(closed clock state))

  (setq org-image-actual-width 600) 

  (setq org-blank-before-new-entry
	'((heading . nil)
	  (plain-list-item . nil)))
  
  ;; showall
  ;(setq org-startup-folded t)

  (setq org-startup-indented t)

  (setq org-capture-templates
        '(("i" "INBOX TODO" entry (file "inbox.org")
           "* %?\n%i")
	  ("b" "BOOKMARK" entry (file "bookmarks.org") "* %?\n:PROPERTIES:\n:Created: %t\n:END:")
	  ("l" "LIFEHACK" entry (file "lifehacks.org") "* %?\n:PROPERTIES:\n:Discovored: %t\n:Source: \n:END:")
	  ("m" "MY (ME)")
	      ("mt" "MY TODO" entry (file+headline "me.org" "TASKS")
           "** TODO %?\nSCHEDULED: %t")
          ;; Capture and keep an org-link to the thing we're currently working with
          ("r" "Capture with Reference" entry (file "inbox.org")
           "* TODO %?\n%U\n%i\n%a")
          ;; Define a section
          
          ("w" "Work" entry (file+headline "health-samurai.org" "INBOX")
           "* %?\n%i")))

    (setq org-agenda-custom-commands
          '(("n" "Agenda and All Todos"
         ((todo "STARTED")
		  (todo "NEXT")
		  (agenda))
	     (;(org-agenda-show-log t)
	      (org-agenda-span 3)
	      (org-agenda-skip-entry-if 'todo 'done)
	      (org-agenda-log-mode-items '(state))
	      (org-agenda-skip-scheduled-if-done t)
	      (org-agenda-skip-timestamp-if-done t)
	      ;(org-agenda-sorting-strategy '((agenda priority-down time-up todo-state-up)))
	      ))
	    ("a" "All TODO items"
	     ((todo "TODO"))
	     ())
		("b" "All TIMEBLOCK items"
	     ((todo "TIMEBLOCK"))
	     ())
        ("w" "Work" agenda ""
         ((org-agenda-files '("health-samurai.org"))))))

    (setq org-agenda-use-time-grid nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Phase 3: extensions (org-roam, etc.)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package org-roam
;;   :ensure t
;;   :config
;;   (org-roam-db-autosync-mode)
;;   ;; Dedicated side window for backlinks
;;   (add-to-list 'display-buffer-alist
;;                '("\\*org-roam\\*"
;;                  (display-buffer-in-side-window)
;;                  (side . right)
;;                  (window-width . 0.4)
;;                  (window-height . fit-window-to-buffer))))

;; Pretty web interface for org-roam
;(use-package org-roam-ui
;  :ensure t
;  :after org-roam
;  :config
;  (setq org-roam-ui-sync-theme t
;        org-roam-ui-follow t
;        org-roam-ui-update-on-save t
;        org-roam-ui-open-on-start t))

(use-package howm
  :ensure t
  :init
  (setq howm-view-title-header "#+TITLE:")
  (setq action-lock-switch-default '("{ }" "{○}" "{◔}" "{◑}" "{◕}" "{●}"))

  :config
  (setq howm-directory "~/org/")
  (setq howm-home-directory "~/org/")
  (setq howm-keyword-file (expand-file-name ".howm-keys" howm-home-directory))
  (setq howm-history-file (expand-file-name ".howm-history" howm-home-directory))
  (setq howm-file-name-format "notes/%Y/%Y-%m-%d-%H%M%S.org")
  (setq howm-template "#+TITLE: %title%cursor"))

(use-package org-timeblock
  :ensure t
  
  :bind
  ("C-c . t" . org-timeblock)

  :config
  ;(add-hook 'org-timeblock-mode-hook (lambda () (scroll-bar-mode -1)))
  )

(use-package org-drill
  :ensure t

  :config
  (setq org-drill-scope 'agenda)
  (setq org-drill-spaced-repetition-algorithm 'sm2))

(use-package org-download
  :ensure t

  :bind
  (("C-c . i" . org-download-clipboard))

  :config
  (setq-default org-download-image-dir "~/org/images"))
