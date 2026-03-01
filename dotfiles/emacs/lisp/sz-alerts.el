;; Notifications helpers

(require 'seq)

(defun sz/notifications-available-p ()
  "Return non-nil when desktop notifications are available."
  (require 'notifications nil 'noerror)
  (fboundp 'notifications-notify))

(defvar sz/notification-ids (make-hash-table :test 'equal))
(defvar sz/mu4e--last-delta-unread 0)

(defun sz/notify (title body &optional category)
  "Send a desktop notification or fall back to `message'."
  (let* ((key (or category "default"))
         (prev-id (gethash key sz/notification-ids 0)))
    (if (sz/notifications-available-p)
        (puthash key
                 (notifications-notify
                  :title title
                  :body body
                  :app-name "emacs"
                  :replaces-id prev-id)
                 sz/notification-ids)
      (message "%s: %s" title body))))

(defun sz/mu4e--notification-item ()
  "Pick a bookmark item to notify about."
  (let* ((items (mu4e-query-items 'bookmarks))
         (new-items (seq-filter (lambda (it)
                                  (> (plist-get it :delta-unread) 0))
                                items))
         (favorite (seq-find (lambda (it) (plist-get it :favorite)) new-items)))
    (or favorite (car new-items))))

(defun sz/mu4e-notification-filter (&optional _)
  "Return non-nil when there's new mail to notify about."
  (not (null (sz/mu4e--notification-item))))

(defun sz/mu4e-notify-new-mail (&optional _)
  "Notify about new mu4e mail via desktop notifications."
  (let* ((item (sz/mu4e--notification-item))
         (delta-unread (if item (or (plist-get item :delta-unread) 0) 0)))
    (cond
     ((and (> delta-unread 0)
           (not (= delta-unread sz/mu4e--last-delta-unread)))
      (setq sz/mu4e--last-delta-unread delta-unread)
      (sz/notify
       "mu4e found new mail"
       (format "%d new message%s in %s"
               delta-unread
               (if (= delta-unread 1) "" "s")
               (plist-get item :name))
       "mu4e"))
     ((zerop delta-unread)
      (setq sz/mu4e--last-delta-unread 0)))))

(provide 'sz-alerts)
