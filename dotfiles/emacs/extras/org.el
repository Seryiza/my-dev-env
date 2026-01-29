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
(setq org-agenda-files '(
			 ;; "~/org"
			 ;; "~/org/areas"
			 ;; "~/org/collections"
			 ;; "~/org/projects"
						 "~/org/todos.org"
						 "~/org/timeblocks.org"
						 "~/org/projects.org"
						 "~/org/areas.org"
						 "~/org/health-samurai.org"
						 "~/org/howm"))

;(setq org-agenda-files (append
;                        '("~/orglife")
;                        (directory-files-recursively "~/orglife/howm" "\\.org\\'")))

					;(setq org-agenda-files (directory-files-recursively "~/org" "\\.org\\'"))

;; Default tags
(setq org-tag-alist '(
					  (:startgroup)
					  ("habit")
					  (:endgroup)
					  (:newline)
					  
                      ;; locale
                      ;; (:startgroup)
                      ;; ("home" . ?h)
                      ;; ("work" . ?w)
                      ;; ("school" . ?s)
                      ;; (:endgroup)
                      ;; (:newline)
                      ;; scale
                      ;; (:startgroup)
                      ;; ("one-shot" . ?o)
                      ;; ("project" . ?j)
                      ;; ("tiny" . ?t)
                      ;; (:endgroup)
                      ;; misc
                      ;; ("meta")
                      ;; ("review")
                      ;; ("reading")
					  ))

(setq org-refile-targets '(;("personal.org" :maxlevel . 1)
                           (nil :level . 1)))
(setq org-archive-location "~/org/archive/%s::")

;;; Phase 3 variables

;; Org-roam variables
					;(setq org-roam-directory "~/orglife/roam")
					;(setq org-roam-index-file "~/orglife/roam/index.org")

;;; Optional variables

;; Advanced: Custom link types
;; This example is for linking a person's 7-character ID to their page on the
;; free genealogy website Family Search.
(setq org-link-abbrev-alist
      '(("family_search" . "https://www.familysearch.org/tree/person/details/%s")))

(defadvice org-agenda (around split-vertically activate)
  (let ((split-width-threshold 80))  ; or whatever width makes sense for you
    ad-do-it))

(defun open-productivity-layout ()
  (interactive)
  (jump-to-register ?p))

(defun open-org-top-headings ()
  (interactive)
  (consult-org-heading "+LEVEL=1"))

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
  ("C-c l C-j" . org-agenda)
  ("C-c l l" . org-capture)
  ("C-c l C-[" . open-productivity-layout)
  ("C-c l C-q" . open-org-top-headings)

  :hook
  (org-mode . org-indent-mode)
  (org-mode . howm-mode)

  :init
  (setq org-modules '(org-habit))

  ;; (setopt org-adapt-indentation 'headline-data)
  (setopt org-insert-heading-respect-content t)
  ;; (setopt org-hide-leading-stars t)
  (setopt org-support-shift-select t)
  
  :config
										;(require 'oc-csl)                     ; citation support
  (add-to-list 'org-export-backends 'md)

  ;; TODO: debug it with my org files (CANX state?)
  (setq org-element-use-cache nil)

  ;; Make org-open-at-point follow file links in the same window
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)

  ;; Make exporting quotes better
  (setq org-export-with-smart-quotes t)

  (setq org-expiry-inactive-timestamps t)

  ;; (setq org-agenda-scheduled-leaders '("" ""))
  (setq org-agenda-prefix-format
		'((agenda . " %-20:c%?-12t% s")
										;(agenda . "  %?-12t% s")
		  ;(agenda . " %?-12t% s")
										;(todo . " %i %-12:c")
		  (todo . " %i")
		  (tags . " %i %-12:c")
		  (search . " %i %-12:c")))

  )

(use-package org-agenda
  :ensure nil
  :after org
  :bind (:map org-agenda-mode-map
              ("g" . my-org-agenda-save-and-redo))
  :config
  (defun my-org-agenda-save-and-redo (&optional arg)
    "Save all Org buffers, then redo the agenda.
ARG is passed to `org-agenda-redo-all'."
    (interactive "P")
    (org-save-all-org-buffers)
    (org-agenda-redo-all arg)))


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
  (setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "STRT(s!)" "WAIT(w!)" "|" "DONE(d)" "CANX(c)")))

  ;; Refile configuration
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-use-outline-path 'file)
  (setq org-link-descriptive nil)
  ;; TODO: improve it
  (setq org-M-RET-may-split-line '((default . nil)))
  ;; (setq org-M-RET-may-split-line '((item . t)))
  ;; TODO: change formatting for LOGBOOK entries
  (setq org-cycle-emulate-tab nil)
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
		  (plain-list-item . auto)))
  
  (setq org-startup-folded 'nofold)

  (setq org-indent-indentation-per-level 2)

  

  (setopt org-capture-templates
		  '(("i" "inbox item" item (file "inbox.org")
			 "%?\n%i")

			("m" "my todo" entry (file+olp "areas.org" "Personal" "Tasks")
   			 "* TODO %?")

			("t" "today todo" entry (file+olp "areas.org" "Personal" "Tasks")
   			 "* TODO %?\nSCHEDULED: %t")

			("b" "timeblock" entry (file+olp "areas.org" "Personal" "Timeblocks")
   			 "* %^{TIMEBLOCK}  :timeblock:\n%^{TIMESTAMP}T"
   			 :immediate-finish t)

			("w" "Work")
			("wb" "timeblock" entry (file+olp "health-samurai.org" "Work" "Timeblocks")
   			 "* %^{TIMESTAMP}T WORK  :timeblock:"
   			 :immediate-finish t
			 :empty-lines 1)))

  ;;   (setq org-capture-templates
  ;;         '(("i" "inbox item" item (file "inbox.org")
  ;;            "%?\n%i")

  ;; 	  ("t" "today todo" entry (file+headline "todos.org" "TODOS")
  ;; 	   "* TODO %?\nSCHEDULED: %t")

  ;; 	  ("b" "timeblock" entry (file+olp "timeblocks.org" "Time Blocks")
  ;; 	   "* %^{TIMESTAMP}T %^{TIMEBLOCK}"
  ;; 	   :immediate-finish t)

  ;; 	  ("d" "diary record" entry (file+olp "diary.org" "Diary Records")
  ;; 	   "* %U %^{NOTE}"
  ;; 	   :immediate-finish t
  ;; 	   :prepend t)

		  
  ;; 	  ;; ("j" "Journal")
  ;; 	  ;; ("jt" "journal todo" entry (file+olp+datetree "areas/journal.org")
  ;; 	  ;;  "* TODO %?\nSCHEDULED: %t")
  ;; 	  ;; ("jn" "journal note" entry (file+olp+datetree "areas/journal.org")
  ;; 	  ;;  "* %U %?")
  ;; 	  ;; ("jb" "journal timeblock" entry (file+olp+datetree "areas/journal.org")
  ;; 	  ;;  "* %^{what}  :timeblock:\n%^{when}T"
  ;; 	  ;;  :immediate-finish t)

  ;; 	  ("h" "health samurai todo" entry (file+headline "areas/health-samurai.org" "Inbox")
  ;; 	   "** %?\n%i")
	       
  ;; 	  ("e" "Two-Direction English Card" entry (file "decks/english.org")
  ;; 	   "* %\\1 :drill:\n** English\n%^{prompt}\n** Definition\n%^{prompt}\n%?\n** Notes\n")
	  
  ;; 	  ;; ("," "Timeblocks")
  ;; 	  ;; (",s" "Sleep" entry (file+olp "timeblocks.org" "timeblocks" "sleep")
  ;; 	  ;;  "* sleep\nSCHEDULED: %^{time}T"
  ;; 	  ;;  :immediate-finish t)
  ;; 	  ;; (",g" "Games" entry (file+olp "timeblocks.org" "timeblocks" "games")
  ;; 	  ;;  "* ^{what}\nSCHEDULED: %^{time}T"
  ;; 	  ;;  :immediate-finish t)
  ;; 	  ;; (",w" "Work" entry (file+olp "timeblocks.org" "timeblocks" "work")
  ;; 	  ;;  "* work\nSCHEDULED: %^{time}T"
  ;; 	  ;;  :immediate-finish t)
  ;; 	  ;; (",," "Other" entry (file+olp "timeblocks.org" "timeblocks" "other")
  ;; 	  ;;  "* %^{what}\nSCHEDULED: %^{when}T"
  ;; 	  ;;  :immediate-finish t)

  ;; 	  ("." "Logs")
  ;; 	  (".d" "Drink" entry (file+olp "logs/drinks.org" "Drinks" "Logs")
  ;; 	   "* %U %^{DRINK|water|black tea|earl gray black tea|kvas|coffee}
  ;; %^{VOLUME_IN_ML}p
  ;; %^{SUGAR_IN_TEASPOONS}p
  ;; %^{NOTE}"
  ;; 	   :immediate-finish t)

  ;; 	  (".s" "Snack" entry (file+olp "logs/snacks.org" "Snacks" "Logs")
  ;; 	   "* %U %^{SNACK|kinder chocolate|snickers}
  ;; %^{WEIGHT_IN_GRAMS}p
  ;; %^{NOTE}"
  ;; 	   :immediate-finish t)

  ;; 	  (".g" "Games" entry (file+olp "logs/games.org" "Games" "Logs")
  ;; 	    "* %U %^{GAME}
  ;; %^{NOTE}"
  ;; 	  :immediate-finish t)
	  
  ;; 	  ;; (".h" "Home" entry (file+olp "logs.org" "logs" "home")
  ;; 	  ;;  "* %T %^{subject} %^{action}"
  ;; 	  ;;  :immediate-finish t)
  ;; 	  ;; (".a" "Health" entry (file+olp "logs.org" "logs" "health")
  ;; 	  ;;  "* %T %^{subject} %^{action}"
  ;; 	  ;;  :immediate-finish t)
  ;; 	  ;; (".f" "Food" entry (file+olp "logs.org" "logs" "food")
  ;; 	  ;;  "* %T %^{subject} %^{action}"
  ;; 	  ;;  :immediate-finish t)
  ;; 	  ;; (".c" "Candy" entry (file+olp "logs.org" "logs" "candy")
  ;; 	  ;;  "* %T %^{subject} %^{action}"
  ;; 	  ;;  :immediate-finish t)
  ;; 	  ;; (".s" "S" entry (file+olp "logs.org" "logs" "s")
  ;; 	  ;;  "* %T %^{subject} %^{action}"
  ;; 	  ;;  :immediate-finish t)
  ;; 	  ;; (".i" "Friends" entry (file+olp "logs.org" "logs" "friends")
  ;; 	  ;;  "* %T %^{subject} %^{action}"
  ;; 	  ;;  :immediate-finish t)
  ;; 	  ;; (".v" "Devices" entry (file+olp "logs.org" "logs" "devices")
  ;; 	  ;;  "* %T %^{subject} %^{action}"
  ;; 	  ;;  :immediate-finish t)
  ;; 	  ;; (".p" "Paper" entry (file+olp "logs.org" "logs" "paper")
  ;; 	  ;;  "* %T %^{subject} %^{action}"
  ;; 	  ;;  :immediate-finish t)
  ;; 	  ;; (".m" "Meditation" entry (file+olp "logs.org" "logs" "meditation")
  ;; 	  ;;  "* %T %^{subject} %^{action}"
  ;; 	  ;;  :immediate-finish t)
  ;; 	  ;; (".t" "Fitness" entry (file+olp "logs.org" "logs" "fitness")
  ;; 	  ;;  "* %T %^{subject} %^{action}"
  ;; 	  ;;  :immediate-finish t)
  ;; 	  ;; (".r" "Reading" entry (file+olp "logs.org" "logs" "reading")
  ;; 	  ;;  "* %T %^{subject} %^{action}"
  ;; 	  ;;  :immediate-finish t)
  ;; 	   ))

  (setopt org-agenda-custom-commands
   		  '(;; Archive tasks
   			("#" "To archive" todo "DONE|CANX")

   			;; Review weekly appointements
   			("$" "Weekly appointments" agenda* "Weekly appointments")

   			;; Review weekly tasks
   			("w" "Week tasks" agenda "Scheduled tasks for this week"
   			 (; (org-agenda-category-filter-preset '("-RDV")) ; RDV for Rendez-vous
			  (org-agenda-tag-filter-preset '("-habit"))
   			  (org-deadline-warning-days 0)
   			  (org-agenda-use-time-grid nil)))

			;; Review daily tasks
			;; ("j" "Today tasks" agenda "Scheduled tasks for today"
   			;;  ((org-agenda-span 1)
   			;;   (org-deadline-warning-days 0)
   			;;   (org-agenda-use-time-grid nil)))
			("j" "Today Journal"
			 ((agenda ""
				  ((org-agenda-span 1)
				   (org-agenda-start-day "0d")
				   (org-agenda-use-time-grid t)
				   (org-agenda-time-grid '((daily today require-timed)
							   (1000 1500 2000 0000)
							   "......" "----------------"))
				   (org-agenda-overriding-header "Timeline / Schedule")))
			  (todo "STRT"
				((org-agenda-overriding-header "In progress")))
			  (todo "NEXT"
				((org-agenda-overriding-header "Next actions")))
			  (todo "WAITING"
				((org-agenda-overriding-header "Waiting / blocked")))))

   			;; Review started and next tasks
   			("s" "STRT/NEXT" tags-todo "TODO={STRT\\|NEXT}")

   			;; Review other non-scheduled/deadlined to-do tasks
   			("t" "TODO" tags-todo "TODO={TODO}+DEADLINE=\"\"+SCHEDULED=\"\"")

   			;; Review other non-scheduled/deadlined pending tasks
   			("w" "WAIT" tags-todo "TODO={WAIT}+DEADLINE=\"\"+SCHEDULED=\"\"")

   			;; Review upcoming deadlines for the next 60 days
   			("!" "Deadlines all" agenda "Past/upcoming deadlines"
   			 ((org-agenda-span 1)
   			  (org-deadline-warning-days 60)
   			  (org-agenda-entry-types '(:deadline))))))

  ;; (setq org-agenda-custom-commands
  ;;       '(("n" "Agenda and All Todos"
  ;;      ((agenda)
  ;;   ;; (todo "ONGO")
  ;;   ;; (todo "NEXT")
  ;;   ; (tags-todo "project")
  ;;   )
	 
  ;;  (				;(org-agenda-show-log t)
  ;; 				;(org-agenda-files (append org-agenda-files '("work/health-samurai.org")))
  ;;   (org-agenda-prefix-format
  ;;    '((agenda . " %t ")
  ;;      (todo . " %i")))
  ;;   (org-agenda-span 'week)
  ;;   (org-agenda-skip-entry-if 'todo 'done)
  ;;   (org-agenda-log-mode-items '(state))
  ;;   (org-agenda-skip-scheduled-if-done t)
  ;;   (org-agenda-skip-timestamp-if-done t)
  ;;   ;; (org-agenda-tag-filter-preset '("-project"))
  ;;   ;; (org-agenda-filter-by-tag "project")
  ;;   ;; (org-agenda-use-tag-inheritance '(agenda))
  ;;   (org-agenda-use-time-grid nil)
  ;;   ;; (org-agenda-include-inactive-timestamps t)
  ;;   ;; (org-agenda-hide-tags-regexp "timeblock")
  ;;   ;; (org-agenda-sorting-strategy '((agenda priority-down time-up todo-state-up)))
  ;;   ))

  ;;     ("p" "Unscheduled Todos"
  ;;      ((todo "ONGO")
  ;; 	  (todo "NEXT")
  ;; 	  (todo "TODO"))
  ;;      ((org-agenda-prefix-format
  ;;    '((todo . " %i")))
  ;; 	  (org-agenda-sorting-strategy '((todo urgency-down tag-up category-keep)))
  ;; 	  (org-agenda-todo-ignore-scheduled 'all)))
		
  ;;     ("w" "Work"
  ;;      ((todo "ONGO")
  ;;       (todo "NEXT")
  ;;       (agenda))
  ;;      ((org-agenda-files '("areas/health-samurai.org"))
  ;; 				; (agenda . " %?-12t% s")
  ;;       (org-agenda-skip-entry-if 'todo 'done)
  ;;       (org-agenda-log-mode-items '(state))
  ;;       (org-agenda-skip-scheduled-if-done t)
  ;;       (org-agenda-skip-timestamp-if-done t)
  ;;       (org-agenda-prefix-format " %i%-20:c%-12t")))

  ;; 	("q" "Work Queue"
  ;;      ((todo "TODO"))
  ;;      ((org-agenda-files '("areas/health-samurai.org"))
  ;; 	  (org-agenda-todo-ignore-scheduled 'all)))))
  )

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
	:defer nil

	;; :hook
	;; (howm-view-contents-mode . howm-org-font-lock-minor-mode)
	;; (howm-view-summary-mode . howm-org-font-lock-minor-mode)
  
	:init
	;; howm + org-mode
	;; (setq howm-file-name-format "%Y-%m-%d-%H%M%S.org")
	(setq howm-view-title-header "*")
	(setq howm-dtime-format "[%Y-%m-%d %a %H:%M]")
	(setq howm-view-title-skip-regexp
		  (concat "\\(^\\*?\\s-*$\\)" "\\|" ;; empty title or ...
				  ;; date & time
				  (concat "\\(^\\[[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}"
						  " .+ " ;; day of the week
						  "[0-9]\\{2\\}:[0-9]\\{2\\}\\]\\)")))
	(setq howm-menu-file-extension ".org")
	(setq howm-menu-skel-replace-rules '(("^= " . "* ") ("^== " . "** ")))

	(setq howm-keyword-body-regexp "[^>=\r\n]+")
	(setq howm-ref-body-regexp "[^=\r\n]+")

	;; Disable wiki link [[...]] for syntax compatibility.
	(setq howm-wiki-regexp nil)

	(setq howm-directory "~/org/")
	(setq howm-file-name-format "howm/%Y-%m-%d-%H%M%S.org")
	(setq howm-prefix (kbd "C-c ,"))
										; (setq howm-view-title-header "#+TITLE:")
										; (setq action-lock-switch-default '("{ }" "{○}" "{◔}" "{◑}" "{◕}" "{●}"))
	(setq howm-keyword-file (expand-file-name ".howm-keys" howm-directory))
	(setq howm-history-file (expand-file-name ".howm-history" howm-directory))
										; (setq howm-view-title-header "*")
										; (setq howm-dtime-format "[%Y-%m-%d %a %H:%M]")
	(setq howm-template "* %title%cursor\n:PROPERTIES:\n:CREATED: %date\n:END:"))

(use-package org-timeblock
  ;; :ensure t

  :quelpa (org-timeblock :fetcher git :url "/home/seryiza/code/org-timeblock")
  
  :bind
  (("C-c . t" . org-timeblock)
   :map org-timeblock-mode-map
   ("C-n" . org-timeblock-forward-block)
   ("C-p" . org-timeblock-backward-block))

  :config
  (setq org-timeblock-span 1)
  (setq org-timeblock-show-future-repeats t)
  (setq org-timeblock-files '("~/org/areas/journal.org"))
  ;(add-hook 'org-timeblock-mode-hook (lambda () (scroll-bar-mode -1)))
  )

;; (use-package org-drill
;;   :ensure t

;;   :config
;;   ;(setq org-drill-scope 'agenda)
;;   (setq org-drill-spaced-repetition-algorithm 'sm2))

(use-package org-download
  :ensure t

  :bind
  (("C-c . c" . org-download-clipboard)
   ("C-c . i" . org-download-image))

  :config
  (setq-default org-download-image-dir "~/org/images"))

(use-package edraw
  :quelpa
  (edraw
   :fetcher github
   :repo "misohena/el-easydraw")

  :config
  (with-eval-after-load 'org
    (require 'edraw-org)
    (edraw-org-setup-default)))

;; (use-package org-modern
;;   :ensure t

;;   :custom
;;   ;(org-modern-fold-stars '(("◉" . "•")))
;;   (org-modern-fold-stars '(("⊙" . "⋅")))

;;   :config
;;   (setq
;;    ;; Edit settings
;;    org-auto-align-tags nil
;;    org-tags-column 0
;;    org-catch-invisible-edits 'show-and-error
;;    org-special-ctrl-a/e t
;;    org-insert-heading-respect-content t

;;    ;; Org styling, hide markup etc.
;;    org-hide-emphasis-markers t
;;    org-pretty-entities t
;;    org-agenda-tags-column 0
;;    org-ellipsis "…")
  
;;   (with-eval-after-load 'org (global-org-modern-mode)))

(defvar my-resume-timer nil
  "Timer for `my-timer-function' to reschedule itself, or nil.")

(defun refresh-org-timeblock ()
  (message nil)
  (org-timeblock-redraw-buffers)
  (text-scale-set -4))

(defun run-background-refreshing ()
  (setq my-resume-timer
	(run-with-timer 0 120 #'refresh-org-timeblock)))

(defun stop-background-refreshing ()
	(cancel-timer my-resume-timer))

(use-package org-pomodoro
  :ensure t
  :bind (("C-c . ;" . org-pomodoro)))

;; (use-package edwina
;;   :ensure t
;;   :custom
;;   (edwina-narrow-threshold 120)
  
;;   :config
;;   (setq display-buffer-base-action '(display-buffer-below-selected))
;;   (edwina-setup-dwm-keys)
;;   (edwina-mode 1))

(defface yant/org-agenda-highlight-project-face `((t :background "#ffebdb" :extend t))
  "Face used to highlight project entries in agenda view.")

(defface yant/org-agenda-highlight-next-face `((t :background "#fff8db" :extend t))
  "Face used to highlight NEXT entries in agenda view.")

(defface yant/org-agenda-highlight-habit-face `((t :background "#f5ffdb" :extend t))
  "Face used to highlight habit entries in agenda view.")

(defface yant/org-agenda-highlight-work-face `((t :background "#fcecee" :extend t))
  "Face used to highlight work entries in agenda view.")

(defun yant/org-agenda-highlight (text face)
  "Highlight TEXT as FACE in agenda."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward text nil t)
	(font-lock-append-text-property (line-beginning-position) (line-end-position) 'face face)))))

(defun yant/org-agenda-highlights ()
  "Highlight items in agenda."
  ;; (yant/org-agenda-highlight ":project:" 'yant/org-agenda-highlight-project-face)
  (yant/org-agenda-highlight ":timeblock:" 'yant/org-agenda-highlight-habit-face)
  (yant/org-agenda-highlight "STRT" 'yant/org-agenda-highlight-work-face)
  (yant/org-agenda-highlight "NEXT" 'yant/org-agenda-highlight-next-face)
  )

(add-hook 'org-agenda-finalize-hook #'yant/org-agenda-highlights)

;; (use-package org-margin
;;   :quelpa (org-margin
;; 		   :fetcher github
;; 		   :repo "rougier/org-margin"))

(defun my-org-electric-pairs ()
  (setq-local electric-pair-pairs
              (append '((?\( . ?\))) electric-pair-pairs))
  (setq-local electric-pair-text-pairs electric-pair-pairs)
  ;; Ensure nothing vetoes '(' pairing.
  (setq-local electric-pair-inhibit-predicate
              (let ((old electric-pair-inhibit-predicate))
                (lambda (c)
                  (if (eq c ?\()
                      nil               ; never inhibit '('
                    (when old (funcall old c)))))))
