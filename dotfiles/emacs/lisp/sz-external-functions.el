;; === External functions

(defun sz/org-waybar-current-timeblock ()
  "Return heading of the timeblock covering now, or nil."
  (let* ((now  (current-time))
         (date (format-time-string "%Y-%m-%d" now))
         (hm   (format-time-string "%H:%M" now))
         (re   (concat "<" date "[^>]* \\([0-9][0-9]:[0-9][0-9]\\)-\\([0-9][0-9]:[0-9][0-9]\\)>")))
    (catch 'done
      (dolist (f (org-agenda-files))
        (with-current-buffer (find-file-noselect f)
          (org-with-wide-buffer
           (goto-char (point-min))
           (while (re-search-forward re nil t)
             (let ((s (match-string 1)) (e (match-string 2)))
               (when (and (not (string< hm s))  ; s <= hm
                          (string< hm e))       ; hm < e
                 (org-back-to-heading t)
                 (throw 'done (substring-no-properties (org-get-heading t t t t)))))))))
      nil)))
