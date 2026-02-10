;; === Org mode

(setq org-directory "~/org")
(setq org-agenda-files '("~/org"))
(setq org-archive-location "~/org/archive/%s::")

(defun sz/meow-org-promote-subtree ()
  (interactive)
  (when (derived-mode-p 'org-mode) (org-promote-subtree)))

(defun sz/meow-org-demote-subtree ()
  (interactive)
  (when (derived-mode-p 'org-mode) (org-demote-subtree)))

(defun sz/meow-org-move-subtree-down ()
  (interactive)
  (when (derived-mode-p 'org-mode) (org-move-subtree-down)))

(defun sz/meow-org-move-subtree-up ()
  (interactive)
  (when (derived-mode-p 'org-mode) (org-move-subtree-up)))

(use-package org
  :hook
  ((org-mode . visual-line-mode)
   (org-agenda-mode . hl-line-mode)
   (org-mode . org-indent-mode))
  :bind
  ("C-c j h" . org-agenda)
  ("C-c j j" . org-capture)
  :config
  ;; Make org-open-at-point follow file links in the same window
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)

  ;; TODO: debug it with my org files (CANX state?)
  (setq org-element-use-cache nil)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "STRT(s!)" "WAIT(w!)" "|" "DONE(d)" "CANX(c)")))

  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-use-outline-path 'file)
  (setq org-link-descriptive nil)

  (setq org-log-into-drawer "LOGBOOK")
  (setq org-log-repeat 'time)
  (setq org-log-done 'time)

  (setq org-image-actual-width 400)
  (setq org-startup-with-inline-images t)
  (setq org-tags-column 40)

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

            ("n" "NEXT todo" entry (file+olp "areas.org" "Personal" "Tasks")
             "* NEXT %?")

            ("b" "timeblock" entry (file+olp "areas.org" "Personal" "Timeblocks")
             "* %^{TIMEBLOCK}  :timeblock:\n%^{TIMESTAMP}T"
             :immediate-finish t)

            ("w" "Work")
            ("wb" "timeblock" entry (file+olp "health-samurai.org" "Work" "Timeblocks")
             "* %^{TIMESTAMP}T WORK  :timeblock:"
             :immediate-finish t
             :empty-lines 1)))

  (setopt org-agenda-custom-commands
          '(;; Archive tasks
            ("#" "To archive" todo "DONE|CANX")

            ;; Review weekly appointments
            ("$" "Weekly appointments" agenda* "Weekly appointments")

            ;; Review weekly tasks
            ("w" "Week tasks" agenda "Scheduled tasks for this week"
             ((org-agenda-tag-filter-preset '("-habit"))
              (org-deadline-warning-days 0)
              (org-agenda-use-time-grid nil)))

            ;; Review daily tasks
            ("h" "Today Journal"
             ((agenda ""
                      ((org-agenda-span 1)
                       (org-agenda-start-day "0d")
                       (org-agenda-use-time-grid t)
                       (org-agenda-overriding-header "Timeline / Schedule")
                       (org-agenda-skip-scheduled-if-done t)
                       (org-agenda-skip-deadline-if-done t)
                       (org-agenda-skip-timestamp-if-done t)
                       (org-agenda-time-grid '((daily today require-timed)
                                               ()
                                               "......" "----------------"))))
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
              (org-agenda-entry-types '(:deadline)))))))

(defun sz/org-agenda-save-and-redo (&optional arg)
  "Save all Org buffers, then redo the agenda.
ARG is passed to `org-agenda-redo-all'."
  (interactive "P")
  (org-save-all-org-buffers)
  (org-agenda-redo-all arg))

(use-package org-agenda
  :after org
  :bind (:map org-agenda-mode-map ("g" . sz/org-agenda-save-and-redo))
  :config
  (define-advice org-agenda (:around (orig-fun &rest args) split-vertically)
    (let ((split-width-threshold 80))
      (apply orig-fun args))))

(provide 'sz-org)
