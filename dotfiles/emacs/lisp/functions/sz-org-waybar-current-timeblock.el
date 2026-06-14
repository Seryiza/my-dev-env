;; === Org Waybar integration

(require 'org)
(require 'org-agenda)
(require 'org-clock)

(defun sz/org-waybar-current-clock ()
  "Return the currently clocked Org heading, or nil."
  (when (and (fboundp 'org-clocking-p)
             (org-clocking-p)
             (bound-and-true-p org-clock-current-task))
    (substring-no-properties org-clock-current-task)))

(defun sz/org-waybar-current-scheduled-timeblock ()
  "Return scheduled timeblock as \"<heading> -> <end-time>\", or nil."
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
             (let ((s (match-string-no-properties 1))
                   (e (match-string-no-properties 2)))
               (when (and (not (string< hm s))  ; s <= hm
                          (string< hm e))       ; hm < e
                 (org-back-to-heading t)
                 (throw 'done
                        (substring-no-properties
                         (format "%s -> %s"
                                 (substring-no-properties (org-get-heading t t t t))
                                 e)))))))))
      nil)))

(defun sz/org-waybar-current-clocking-todo ()
  "Return the active Org clock TODO heading, or nil."
  (sz/org-waybar-current-clock))
