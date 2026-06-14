;; === Telega media save

(defcustom sz/telega-media-save-dir (expand-file-name "~/attachments/telega/")
  "Directory where `sz/telega-save-msg-media' stores files."
  :type 'directory)

(defcustom sz/telega-media-save-timeout 300
  "Seconds to wait for a media download in `sz/telega-save-msg-media-sync'."
  :type 'integer)

(defvar sz/telega--save-dir-by-id (make-hash-table :test #'eql))
(defvar sz/telega--sync-save-state-by-token (make-hash-table :test #'eq))

(defun sz/telega--safe-filename (name)
  "Return a safe basename from NAME, or nil if it looks invalid."
  (when (and (stringp name) (> (length name) 0))
    (let* ((trimmed (directory-file-name name))
           (base (file-name-nondirectory trimmed)))
      (unless (member base '("" "." ".."))
        base))))

(defun sz/telega--guess-filename (msg file)
  "Return a filename (with extension) for FILE from MSG."
  (let* ((id (plist-get file :id))
         (c  (plist-get msg :content))
         (ct (plist-get c :@type)))
    (or
     ;; Types that carry original file_name in TDLib.
     (sz/telega--safe-filename
      (pcase ct
        ("messageDocument"  (plist-get (plist-get c :document) :file_name))
        ("messageAudio"     (plist-get (plist-get c :audio) :file_name))
        ("messageVideo"     (plist-get (plist-get c :video) :file_name))
        ("messageAnimation" (plist-get (plist-get c :animation) :file_name))))
     ;; Fallbacks for types without file_name.
     (pcase ct
       ;; photoSize is JPEG in TDLib -> .jpg is a reasonable default.
       ("messagePhoto" (format "%d.jpg" id))
       ;; sticker format determines extension (webp/tgs/webm).
       ("messageSticker"
        (format "%d.%s" id
                (pcase (plist-get (plist-get (plist-get c :sticker) :format) :@type)
                  ("stickerFormatWebp" "webp")
                  ("stickerFormatTgs" "tgs")
                  ("stickerFormatWebm" "webm")
                  (_ "webp"))))
       ;; videoNote is MPEG4.
       ("messageVideoNote" (format "%d.mp4" id))
       ;; voiceNote mime types are usually ogg/mp3/m4a.
       ("messageVoiceNote"
        (format "%d.%s" id
                (pcase (plist-get (plist-get c :voice_note) :mime_type)
                  ("audio/ogg" "ogg")
                  ("audio/mpeg" "mp3")
                  ("audio/mp4" "m4a")
                  (_ "ogg"))))
       (_ (number-to-string id))))))

(defun sz/telega--msg-timestamp (msg)
  "Return MSG timestamp formatted for filenames."
  (let ((date (plist-get msg :date)))
    (format-time-string "%Y-%m-%d-%H%M%S"
                        (if (numberp date)
                            (seconds-to-time date)
                          (current-time)))))

(defun sz/telega--copy-downloaded-file (file)
  "Copy FILE to its queued destination once Telega finishes downloading it."
  (let* ((id (plist-get file :id))
         (dst (gethash id sz/telega--save-dir-by-id))
         (local (plist-get file :local))
         (src (plist-get local :path))
         (downloading (plist-get local :is_downloading_active))
         (completed (plist-get local :is_downloading_completed))
         (downloaded (plist-get local :downloaded_size))
         (expected (plist-get file :expected_size)))
    (when (and (stringp dst)
               (stringp src)
               (> (length src) 0)
               (file-regular-p src)
               (not downloading)
               (or completed
                   (and (numberp expected)
                        (> expected 0)
                        (numberp downloaded)
                        (>= downloaded expected))))
      (if (file-directory-p dst)
          (progn
            (remhash id sz/telega--save-dir-by-id)
            (message "Skip save: destination is a directory: %s"
                     (abbreviate-file-name dst))
            nil)
        (copy-file src dst t)
        (remhash id sz/telega--save-dir-by-id)
        (message "Saved %s" (abbreviate-file-name dst))
        dst))))

(defun sz/telega--media-save-destination (dir msg file)
  "Return the save destination in DIR for FILE from MSG."
  (expand-file-name
   (format "%s--%s"
           (sz/telega--msg-timestamp msg)
           (sz/telega--guess-filename msg file))
                    (file-name-as-directory dir)))

(defun sz/telega--sync-save-callback (token file)
  "Update TOKEN state after FILE download progress."
  (let* ((saved (sz/telega--copy-downloaded-file file))
         (state (gethash token sz/telega--sync-save-state-by-token)))
    (when (and saved state)
      (puthash token
               (list :done t
                     :result saved)
               sz/telega--sync-save-state-by-token))))

(defun sz/telega-save-msg-media (&optional dir msg)
  "Download (if needed) and copy message-at-point file into DIR."
  (interactive (list nil (telega-msg-for-interactive)))
  (let* ((dir (file-name-as-directory (or dir sz/telega-media-save-dir)))
         (_ (make-directory dir t))
         (file (telega-msg--content-file msg)))
    (unless file
      (user-error "No file in this message"))
    (puthash (plist-get file :id)
             (sz/telega--media-save-destination dir msg file)
             sz/telega--save-dir-by-id)
    (or (sz/telega--copy-downloaded-file file)
        (telega-file--download
         file
         :priority 32
         :update-callback #'sz/telega--copy-downloaded-file))))

(defun sz/telega-save-msg-media-sync (&optional dir msg)
  "Download (if needed) and copy message-at-point file into DIR, waiting for completion.
Return the saved file path."
  (let* ((dir (file-name-as-directory (or dir sz/telega-media-save-dir)))
         (_ (make-directory dir t))
         (msg (or msg (telega-msg-for-interactive)))
         (file (telega-msg--content-file msg))
         (id (and file (plist-get file :id)))
         (token (make-symbol "sz-telega-sync-save"))
         (result nil))
    (unless file
      (user-error "No file in this message"))
    (puthash id
             (sz/telega--media-save-destination dir msg file)
             sz/telega--save-dir-by-id)
    (setq result (sz/telega--copy-downloaded-file file))
    (unless result
      (puthash token
               (list :done nil
                     :result nil)
               sz/telega--sync-save-state-by-token)
      ;; Wait in the command loop so the update callback can finish the copy.
      (telega-file--download
       file
       :priority 32
       :update-callback
       `(lambda (updated-file)
          (sz/telega--sync-save-callback ',token updated-file)))
      (with-timeout
          (sz/telega-media-save-timeout
           (remhash token sz/telega--sync-save-state-by-token)
           (remhash id sz/telega--save-dir-by-id)
           (user-error "Timed out after %s seconds while downloading media"
                       sz/telega-media-save-timeout))
        (while (not (plist-get (gethash token sz/telega--sync-save-state-by-token) :done))
          (accept-process-output nil 0.1)))
      (setq result (plist-get (gethash token sz/telega--sync-save-state-by-token) :result))
      (remhash token sz/telega--sync-save-state-by-token))
    result))

(provide 'sz-telega-save-msg-media)
