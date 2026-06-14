;; === Org timeblock capture

(require 'subr-x)

(defconst sz/org-personal-timeblocks-file "areas/personal.org")
(defconst sz/org-personal-timeblocks-path '("Personal"))
(defconst sz/org-timeblock--hour-re "\\(?:[0-9]\\|[01][0-9]\\|2[0-3]\\)")

(defun sz/org-timeblock--parse-clock (value)
  "Parse VALUE in H:MM or HH:MM format and return minutes since midnight."
  (unless (string-match (concat "\\`\\(" sz/org-timeblock--hour-re "\\):\\([0-5][0-9]\\)\\'") value)
    (user-error "Invalid time: %s" value))
  (+ (* 60 (string-to-number (match-string 1 value)))
     (string-to-number (match-string 2 value))))

(defun sz/org-timeblock--parse-duration (value)
  "Parse VALUE in H or H:MM format and return minutes."
  (cond
   ((string-match "\\`\\([0-9]+\\)\\'" value)
    (* 60 (string-to-number (match-string 1 value))))
   ((string-match "\\`\\([0-9]+\\):\\([0-5][0-9]\\)\\'" value)
    (+ (* 60 (string-to-number (match-string 1 value)))
       (string-to-number (match-string 2 value))))
   (t
    (user-error "Invalid duration: %s" value))))

(defun sz/org-timeblock--format-clock (minutes)
  "Format MINUTES since midnight as HH:MM."
  (format "%02d:%02d" (/ minutes 60) (% minutes 60)))

(defun sz/org-timeblock--parse-input (input)
  "Parse INPUT into a plist with :start :end and :title."
  (let ((trimmed (string-trim input)))
    (cond
     ((string-match
       (concat "\\`\\(" sz/org-timeblock--hour-re "\\):\\([0-5][0-9]\\)[+]\\([0-9]+\\(?::[0-5][0-9]\\)?\\)\\s-+\\(.+\\S-\\)\\'")
       trimmed)
      (let* ((start-text (format "%s:%s" (match-string 1 trimmed) (match-string 2 trimmed)))
             (duration-text (match-string 3 trimmed))
             (title (match-string 4 trimmed))
             (start (sz/org-timeblock--parse-clock start-text))
             (duration (sz/org-timeblock--parse-duration duration-text))
             (end (+ start duration)))
        (when (= duration 0)
          (user-error "Duration must be greater than zero"))
        (when (> end (* 24 60))
          (user-error "Timeblock ends after 24:00"))
        (list :start start :end end :title title)))
     ((string-match
       (concat "\\`\\(" sz/org-timeblock--hour-re "\\):\\([0-5][0-9]\\)-"
               "\\(" sz/org-timeblock--hour-re "\\):\\([0-5][0-9]\\)\\s-+\\(.+\\S-\\)\\'")
       trimmed)
      (let* ((start-text (format "%s:%s" (match-string 1 trimmed) (match-string 2 trimmed)))
             (end-text (format "%s:%s" (match-string 3 trimmed) (match-string 4 trimmed)))
             (title (match-string 5 trimmed))
             (start (sz/org-timeblock--parse-clock start-text))
             (end (sz/org-timeblock--parse-clock end-text)))
        (when (<= end start)
          (user-error "End time must be after start time"))
        (list :start start :end end :title title)))
     (t
      (user-error "Use H:MM+N title, H:MM+H:MM title, or H:MM-HH:MM title")))))

(defun sz/org-timeblock--insert (date entry)
  "Insert ENTRY for DATE under personal timeblocks."
  (let ((file (expand-file-name sz/org-personal-timeblocks-file org-directory))
        (title (plist-get entry :title))
        (start (sz/org-timeblock--format-clock (plist-get entry :start)))
        (end (sz/org-timeblock--format-clock (plist-get entry :end))))
    (with-current-buffer (find-file-noselect file)
      (org-with-wide-buffer
       (goto-char (point-min))
       (let ((pos (org-find-olp sz/org-personal-timeblocks-path t)))
         (unless pos
           (user-error "Missing heading path: %s" (string-join sz/org-personal-timeblocks-path " > ")))
         (goto-char pos))
       (org-back-to-heading t)
       (let ((level (1+ (org-outline-level))))
         (org-end-of-subtree t t)
         (unless (bolp)
           (insert "\n"))
         (let ((heading-pos (point)))
           (insert (make-string level ?*) " " title "\n")
           (save-excursion
             (goto-char heading-pos)
             (org-set-tags '("timeblock"))))
         (insert "<" date " " start "-" end ">\n")
         (save-buffer))))))

(defun sz/org-capture-timeblocks-loop (&optional arg)
  "Capture personal timeblocks until minibuffer input is empty.
With prefix ARG, prompt for the target date first."
  (interactive "P")
  (let* ((date-time (if arg
                        (org-read-date nil t nil "Date: ")
                      (current-time)))
         (date (format-time-string "%Y-%m-%d %a" date-time))
         (count 0)
         done)
    (while (not done)
      (let ((input (string-trim
                    (read-from-minibuffer
                     (format "%s timeblock: " date)))))
        (if (string-empty-p input)
            (setq done t)
          (condition-case err
              (progn
                (sz/org-timeblock--insert date (sz/org-timeblock--parse-input input))
                (when (fboundp 'org-agenda-maybe-redo)
                  (org-agenda-maybe-redo))
                (setq count (1+ count)))
            (user-error
             (message "%s" (cadr err))
             (sit-for 1))))))
    (when (> count 0)
      (org-save-all-org-buffers)
      (when (fboundp 'sz/org-appt-refresh)
        (sz/org-appt-refresh)))
    (message "Captured %d timeblock%s" count (if (= count 1) "" "s"))))

(provide 'sz-org-capture-timeblocks-loop)
