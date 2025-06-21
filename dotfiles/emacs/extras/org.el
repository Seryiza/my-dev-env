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

;; TODO: fix howm directory
(setq org-agenda-files '("~/org"
			 "~/org/notes/2025"))

;(setq org-agenda-files (append
;                        '("~/orglife")
;                        (directory-files-recursively "~/orglife/howm" "\\.org\\'")))

					;(setq org-agenda-files (directory-files-recursively "~/org" "\\.org\\'"))

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

(setq org-refile-targets '(;("personal.org" :maxlevel . 1)
                           (nil :level . 1)))
(setq org-archive-location "archive/%s::")

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

(defun work-todos ()
  (interactive)
  (org-agenda nil "q"))

(defun personal-todos ()
  (interactive)
  (org-agenda nil "p"))

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
   ("C-c . C-p" . personal-todos)
   ("C-c . C-q" . work-todos))
  
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
  ; (setq org-todo-keywords '((sequence "TODO(t)" "STARTED(s!)" "WAITING(w!)" "NEXT(n)" "MEET(m)" "TIMEBLOCK(b)" "|" "DONE(d)" "CANCELED(c)")))
  (setq org-todo-keywords '((sequence "TODO(t)" "ONGO(o!)" "WAIT(w!)" "NEXT(n!)" "MEET(m)" "|" "DONE(d)" "SKIP(s)")))

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

  (setq org-image-actual-width 400)
  (setq org-startup-with-inline-images t)

  (setq org-blank-before-new-entry
		'((heading . nil)
		  (plain-list-item . nil)))
  
  ;; showall
  ;(setq org-startup-folded t)

  (setq org-startup-indented t)
  (setq org-indent-indentation-per-level 6)

  (setq org-capture-templates
        '(("p" "Personal")
		  ("pp" "Inbox Item" entry (file+headline "personal.org" "Inbox")
           "* %?\n%i")
		  
		  ("pt" "Today Todo" entry (file+headline "personal.org" "Tasks")
		   "** TODO %?\nSCHEDULED: %t")

		  ("h" "Health Samurai")
          ("hh" "Inbox Item" entry (file+headline "work/health-samurai.org" "Inbox")
           "* %?\n%i")
		  ("ht" "Today Todo" entry (file+headline "work/health-samurai.org" "Tasks")
		   "** TODO %?\nSCHEDULED: %t")

		  ("e" "English")
		  ("ee" "Two-Direction English Card" entry (file "decks/english.org")
		   "* %\\1 :drill:\n** English\n%^{prompt}\n** Definition\n%^{prompt}\n%?\n** Notes\n")))

    (setq org-agenda-custom-commands
          '(("n" "Agenda and All Todos"
         ((todo "ONGO")
		  (todo "NEXT")
		  (agenda))
	     (;(org-agenda-show-log t)
	      ;(org-agenda-files (append org-agenda-files '("work/health-samurai.org")))
	      (org-agenda-prefix-format '((agenda . " %-12t")
									  (todo . " %i")))
	      (org-agenda-span 1)
	      (org-agenda-skip-entry-if 'todo 'done)
	      (org-agenda-log-mode-items '(state))
	      (org-agenda-skip-scheduled-if-done t)
	      (org-agenda-skip-timestamp-if-done t)
	      ;(org-agenda-sorting-strategy '((agenda priority-down time-up todo-state-up)))
	      ))

	    ("p" "Personal Todos"
	     ((todo "TODO"))
	     ((org-agenda-todo-ignore-scheduled 'all)))
		
	    ("w" "Work" agenda ""
	     ((org-agenda-files '("work/health-samurai.org"))
		  ; (agenda . " %?-12t% s")
	      (org-agenda-prefix-format " %i%-20:c%-12t")))

		("q" "Work Queue"
	     ((todo "TODO"))
	     ((org-agenda-files '("work/health-samurai.org"))
		  (org-agenda-todo-ignore-scheduled 'all)))))

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
  ;(setq org-drill-scope 'agenda)
  (setq org-drill-spaced-repetition-algorithm 'sm2))

(use-package org-download
  :ensure t

  :bind
  (("C-c . c" . org-download-clipboard)
   ("C-c . i" . org-download-image))

  :config
  (setq-default org-download-image-dir "~/org/images"))

(use-package org-modern
  :ensure t

  :config
  (setq
   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-pretty-entities t
   org-agenda-tags-column 0
   org-ellipsis "…")
  
  (with-eval-after-load 'org (global-org-modern-mode)))
