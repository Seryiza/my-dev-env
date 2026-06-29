;; === Telega message capture to Org inbox

(require 'ol)
(require 'org-capture)
(require 'subr-x)
(require 'telega)

(declare-function sz/telega-save-msg-media-sync "sz-telega-save-msg-media")

(defcustom sz/telega-org-capture-inbox-file (expand-file-name "~/org/inbox.org")
  "Org inbox file used by `sz/telega-capture-message-to-inbox'."
  :type 'file
  :group 'org)

(defcustom sz/telega-org-capture-inbox-heading "Inbox"
  "Heading in `sz/telega-org-capture-inbox-file' for telega captures."
  :type 'string
  :group 'org)

(defcustom sz/telega-capture-skip-link-preview-media t
  "Non-nil means do not save media that only comes from a Telegram link preview.

Telega's `telega-msg--content-file' intentionally checks both real message
content and `:content :link_preview :type'.  This option keeps capture focused
on real attachments by skipping media lookup for text messages, where Telegram
stores URL preview images/videos."
  :type 'boolean
  :group 'org)

(defcustom sz/telega-capture-delete-on-success t
  "Non-nil means delete the telega message after a successful Org capture.

Deletion is local-only where Telegram allows it; telega/TDLib may still force
revocation in chat types where delete-for-me is not available."
  :type 'boolean
  :group 'org)

(defun sz/telega--org-file-link (path)
  "Return an Org file link for PATH."
  (org-link-make-string
   (concat "file:" (expand-file-name path))
   (file-name-nondirectory path)))

(defun sz/telega--media-links-section (files)
  "Return an Org list of local file links for FILES, or nil."
  (when files
    (mapconcat
     (lambda (file)
       (concat "- " (sz/telega--org-file-link file)))
     files
     "\n")))

(defun sz/telega--message-text-with-link-preview-p (msg)
  "Return non-nil when MSG is text carrying Telegram link-preview data."
  (let ((content (plist-get msg :content)))
    (and (equal (plist-get content :@type) "messageText")
         (plist-get content :link_preview))))

(defun sz/telega--message-media-files (msg)
  "Save real attachment media from MSG and return downloaded file paths.

Do not save Telegram link-preview media for text messages; those previews are
metadata for URLs, not message attachments."
  (delq nil
        (list
         (unless (and sz/telega-capture-skip-link-preview-media
                      (sz/telega--message-text-with-link-preview-p msg))
           (when (telega-msg--content-file msg)
             (sz/telega-save-msg-media-sync nil msg))))))

(defun sz/telega--formatted-text-as-org (fmt-text)
  "Return FMT-TEXT as Org text, or nil when it has no text.

TDLib sometimes supplies empty formattedText caption objects for media
messages.  `telega--fmt-text-org' expects both :text and :entities, so
normalize missing entities and ignore objects without string text."
  (when-let ((text (plist-get fmt-text :text)))
    (when (stringp text)
      (let ((fmt-text (copy-tree fmt-text)))
        (unless (vectorp (plist-get fmt-text :entities))
          (plist-put fmt-text :entities []))
        (telega--desurrogate-apply
         (telega--fmt-text-org fmt-text))))))

(defun sz/telega--message-content-as-org (msg)
  "Return MSG content or caption formatted as Org text, or an empty string."
  (let* ((content (plist-get msg :content))
         (fmt-text (or (plist-get content :text)
                       (plist-get content :caption)))
         (text
          (cond
           (fmt-text
            (sz/telega--formatted-text-as-org fmt-text))
           ((telega-msg-content-text msg 'with-voice-note)
            (substring-no-properties
             (telega-msg-content-text msg 'with-voice-note))))))
    (string-trim-right (or text ""))))

(defun sz/telega--first-line-and-rest (text)
  "Return (FIRST . REST) from TEXT, trimming blank edge lines."
  (let* ((trimmed (string-trim text))
         (lines (split-string trimmed "\n")))
    (cons (or (car lines) "")
          (string-trim-right (string-join (cdr lines) "\n")))))

(defun sz/telega--capture-headline (text media-files)
  "Return an Org heading title for TEXT and MEDIA-FILES."
  (let ((headline (string-trim
                   (replace-regexp-in-string
                    "[[:space:]\n\r]+" " "
                    (car (sz/telega--first-line-and-rest text))))))
    (cond
     ((not (string-empty-p headline)) headline)
     (media-files "Telega media")
     (t "Telega message"))))

(defun sz/telega--capture-properties-string (_msg)
  "Return Org property drawer with capture metadata."
  (concat ":PROPERTIES:\n"
          (format ":ADDED: %s\n"
                  (format-time-string "[%Y-%m-%d %a %H:%M]"))
          ":SOURCE: telega\n"
          ":END:"))

(defun sz/telega--capture-entry-string (msg text media-files)
  "Return full Org capture entry for MSG with TEXT and MEDIA-FILES."
  (let* ((headline (sz/telega--capture-headline text media-files))
         (text-parts (sz/telega--first-line-and-rest text))
         (rest-text (cdr text-parts))
         (media-links (sz/telega--media-links-section media-files))
         (body (string-join
                (delq nil
                      (list (and (not (string-empty-p rest-text)) rest-text)
                            media-links))
                "\n\n")))
    (concat (format "** %s\n" headline)
            (sz/telega--capture-properties-string msg)
            (if (string-empty-p body)
                ""
              (concat "\n\n" body)))))

(defun sz/telega--capture-one-message-to-inbox (msg)
  "Save media from MSG, capture MSG to the Org inbox, and maybe delete it."
  (let ((was-marked (telega-msg-marked-p msg)))
    (unless was-marked
      (save-excursion
        (telega-msg-mark-toggle msg)))
    (let* ((text (sz/telega--message-content-as-org msg))
           (media-files (sz/telega--message-media-files msg))
           (entry (sz/telega--capture-entry-string msg text media-files))
           (org-capture-templates
            `(("I" "Telega inbox message" entry
               (file+olp ,sz/telega-org-capture-inbox-file
                         ,sz/telega-org-capture-inbox-heading)
               "%i"
               :immediate-finish t))))
      (let ((deleted-p nil))
        (unwind-protect
            (condition-case err
                (let ((captured-p (org-capture-string entry "I")))
                  (when (and captured-p sz/telega-capture-delete-on-success)
                    ;; Clear the mark before deletion so telega's mode line does
                    ;; not keep a stale "marked" count for the deleted message.
                    (ignore-errors
                      (telega-msg-unmark msg))
                    (telega-msg-delete0 msg nil)
                    (setq deleted-p t))
                  captured-p)
              (error
               (message "telega capture failed: %s" (error-message-string err))
               (signal (car err) (cdr err))))
          (when (and (not was-marked)
                     (not deleted-p)
                     (telega-msg-marked-p msg))
            (ignore-errors
              (telega-msg-unmark msg))))))))

(defun sz/telega--marked-messages-sorted ()
  "Return marked telega messages sorted by message id, or nil."
  (when telega-chatbuf--marked-messages
    (sort (copy-sequence telega-chatbuf--marked-messages)
          (lambda (a b)
            (< (plist-get a :id) (plist-get b :id))))))

;;;###autoload
(defun sz/telega-capture-message-to-inbox (&optional msg)
  "Capture telega messages to the Org inbox.

When MSG is non-nil, capture that single message.  When called interactively in
a telega chat with marked messages, capture marked messages one by one in
message-id order.  Otherwise capture the message at point.

Each capture is saved immediately under `sz/telega-org-capture-inbox-heading'
in `sz/telega-org-capture-inbox-file' as a level-2 entry.  Media files are
saved with `sz/telega-save-msg-media-sync' and inserted as local Org file links.
When `sz/telega-capture-delete-on-success' is non-nil, delete each message after
its Org capture succeeds."
  (interactive)
  (unless (derived-mode-p 'telega-chat-mode)
    (user-error "Not in a telega chat buffer"))
  (let ((messages (or (and msg (list msg))
                      (sz/telega--marked-messages-sorted)
                      (when-let ((point-msg (or (telega-msg-at (point))
                                                (ignore-errors
                                                  (telega-msg-for-interactive)))))
                        (list point-msg)))))
    (unless messages
      (user-error "No telega message at point"))
    (let ((captured-count 0))
      (dolist (message messages)
        (when (sz/telega--capture-one-message-to-inbox message)
          (setq captured-count (1+ captured-count))))
      (message "Captured %d telega message%s to Org inbox"
               captured-count
               (if (= captured-count 1) "" "s"))
      captured-count)))

(provide 'sz-telega-capture-message-to-inbox)
