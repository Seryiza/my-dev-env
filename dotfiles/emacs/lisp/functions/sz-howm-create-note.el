;; === Howm note creation

(require 'ol)
(require 'subr-x)

(declare-function sz/telega-copy-marked-messages-as-org "sz-telega")
(declare-function sz/telega-save-msg-media-sync "sz-telega-save-msg-media")

(defun sz/howm--telega-marked-messages ()
  "Return marked telega messages sorted by message id."
  (unless (derived-mode-p 'telega-chat-mode)
    (user-error "Not in a telega chat buffer"))
  (unless telega-chatbuf--marked-messages
    (user-error "No marked messages"))
  (sort (copy-sequence telega-chatbuf--marked-messages)
        (lambda (a b)
          (< (plist-get a :id) (plist-get b :id)))))

(defun sz/howm--source-context (&optional buffer)
  "Collect source metadata for BUFFER or the current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((file (buffer-file-name))
           (line (line-number-at-pos (point) t))
           (chat-title
            (when (and (derived-mode-p 'telega-chat-mode)
                       (boundp 'telega-chatbuf--chat)
                       telega-chatbuf--chat)
              (plist-get telega-chatbuf--chat :title)))
           (origin
            (cond
             (chat-title
              (format "telega chat %s" chat-title))
             (file
              (format "%s:%d" (abbreviate-file-name file) line))
             (t
              (buffer-name)))))
      (list :origin origin
            :buffer-name (buffer-name)
            :major-mode (symbol-name major-mode)
            :file file
            :line line
            :chat-title chat-title))))

(defun sz/howm--property-value (value)
  "Return VALUE as a single-line string suitable for Org properties."
  (replace-regexp-in-string
   "[[:space:]\n\r]+"
   " "
   (string-trim (format "%s" value))))

(defun sz/howm--source-properties-string (source)
  "Return SOURCE metadata formatted as Org property lines."
  (let (lines)
    (dolist (entry `(("SOURCE_ORIGIN" . ,(plist-get source :origin))
                     ("SOURCE_BUFFER" . ,(plist-get source :buffer-name))
                     ("SOURCE_MODE" . ,(plist-get source :major-mode))
                     ("SOURCE_FILE" . ,(plist-get source :file))
                     ("SOURCE_LINE" . ,(plist-get source :line))
                     ("SOURCE_CHAT" . ,(plist-get source :chat-title))))
      (when-let ((value (cdr entry)))
        (push (format ":%s: %s\n"
                      (car entry)
                      (sz/howm--property-value value))
              lines)))
    (apply #'concat (nreverse lines))))

(defun sz/howm--insert-source-properties (source)
  "Insert SOURCE metadata into the first property drawer in the buffer."
  (let ((props (sz/howm--source-properties-string source)))
    (when (not (string-empty-p props))
      (save-excursion
        (goto-char (point-min))
        (if (re-search-forward "^:PROPERTIES:\n" nil t)
            (let ((end (save-excursion
                         (re-search-forward "^:END:\n?" nil t)
                         (match-beginning 0))))
              (goto-char end)
              (insert props))
          (goto-char (point-max))
          (unless (bolp)
            (insert "\n"))
          (insert ":PROPERTIES:\n" props ":END:\n"))))))

(defun sz/howm--file-link (path)
  "Return an Org file link for PATH."
  (org-link-make-string
   (concat "file:" (expand-file-name path))
   (file-name-nondirectory path)))

(defun sz/howm--media-links-section (files)
  "Return an Org list of file links for FILES, or nil."
  (when files
    (concat "Files:\n"
            (mapconcat
             (lambda (file)
               (concat "- " (sz/howm--file-link file)))
             files
             "\n"))))

(defun sz/howm--telega-marked-messages-media-files (messages)
  "Save media from MESSAGES and return downloaded file paths."
  (delq nil
        (mapcar
         (lambda (msg)
           (when (telega-msg--content-file msg)
             (sz/telega-save-msg-media-sync nil msg)))
         messages)))

(defun sz/howm--telega-note-body (messages)
  "Return Org body for MESSAGES, including links to downloaded media."
  (let* ((media-files (sz/howm--telega-marked-messages-media-files messages))
         (text-body
          (condition-case err
              (sz/telega-copy-marked-messages-as-org)
            (user-error
             ;; Keep media-only selections useful by allowing an empty text body.
             (if media-files
                 ""
               (signal (car err) (cdr err))))))
         (media-links (sz/howm--media-links-section media-files)))
    (string-join
     (delq nil
           (list (and (not (string-empty-p text-body)) text-body)
                 media-links))
     "\n\n")))

(defun sz/howm-create-note (title body &optional source)
  "Create a new howm note with TITLE, BODY and SOURCE metadata.
When called interactively, use active region as BODY if available,
otherwise prompt for it, and collect SOURCE from the current buffer."
  (interactive
   (list (read-string "Howm title: ")
         (if (use-region-p)
             (buffer-substring-no-properties (region-beginning) (region-end))
           (read-from-minibuffer "Howm body: "))
         (sz/howm--source-context)))
  (let* ((source (or source (sz/howm--source-context)))
         (body (string-trim-right (or body "")))
         (content (if (string-empty-p body)
                      ""
                    (concat "\n\n" body))))
    (howm-create-file-with-title title nil nil nil content)
    (sz/howm--insert-source-properties source)
    (save-buffer)
    (current-buffer)))

(defun sz/howm-create-note-from-telega-marked-messages (title)
  "Create a new howm note from marked telega messages using TITLE."
  (interactive (list (read-string "Howm title: ")))
  (let* ((messages (sz/howm--telega-marked-messages))
         (source (sz/howm--source-context))
         (body (sz/howm--telega-note-body messages)))
    (sz/howm-create-note title body source)))

(provide 'sz-howm-create-note)
