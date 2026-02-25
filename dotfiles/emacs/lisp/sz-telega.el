;; === Telega

(defun sz/keymap-clear! (map)
  "Remove MAP's own bindings in-place, preserving its parent."
  (let ((parent (keymap-parent map)))
    (setcdr map nil)
    (set-keymap-parent map parent)))

(defvar sz/telega-msg-button-map--orig nil
  "Saved copy of telega's original message-button keymap.")

(defcustom sz/telega-media-save-dir (expand-file-name "~/Downloads/telega/")
  "Directory where `sz/telega-save-msg-media' stores files."
  :type 'directory)

(defvar sz/telega--save-dir-by-id (make-hash-table :test #'eql))

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
     ;; Types that carry original file_name in TDLib
     (sz/telega--safe-filename
      (pcase ct
        ("messageDocument"  (plist-get (plist-get c :document)  :file_name))
        ("messageAudio"     (plist-get (plist-get c :audio)     :file_name))
        ("messageVideo"     (plist-get (plist-get c :video)     :file_name))
        ("messageAnimation" (plist-get (plist-get c :animation) :file_name))))
     ;; Fallbacks for types without file_name
     (pcase ct
       ;; photoSize is JPEG in TDLib -> .jpg is a reasonable default
       ("messagePhoto" (format "%d.jpg" id))
       ;; sticker format determines extension (webp/tgs/webm)
       ("messageSticker"
        (format "%d.%s" id
                (pcase (plist-get (plist-get (plist-get c :sticker) :format) :@type)
                  ("stickerFormatWebp" "webp")
                  ("stickerFormatTgs"  "tgs")
                  ("stickerFormatWebm" "webm")
                  (_ "webp"))))
       ;; videoNote is MPEG4
       ("messageVideoNote" (format "%d.mp4" id))
       ;; voiceNote mime types are usually ogg/mp3/m4a
       ("messageVoiceNote"
        (format "%d.%s" id
                (pcase (plist-get (plist-get c :voice_note) :mime_type)
                  ("audio/ogg"  "ogg")
                  ("audio/mpeg" "mp3")
                  ("audio/mp4"  "m4a")
                  (_ "ogg"))))
       (_ (number-to-string id))))))

(defun sz/telega--copy-downloaded-file (file)
  (let* ((id   (plist-get file :id))
         (dst  (gethash id sz/telega--save-dir-by-id))
         (local (plist-get file :local))
         (src   (plist-get local :path))
         (downloading (plist-get local :is_downloading_active))
         (completed   (plist-get local :is_downloading_completed))
         (downloaded  (plist-get local :downloaded_size))
         (expected    (plist-get file :expected_size)))
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
        t))))

(defun sz/telega-save-msg-media (&optional dir msg)
  "Download (if needed) and copy message-at-point file into DIR."
  (interactive (list nil (telega-msg-for-interactive)))
  (let* ((dir  (file-name-as-directory (or dir sz/telega-media-save-dir)))
         (_    (make-directory dir t))
         (file (telega-msg--content-file msg)))
    (unless file (user-error "No file in this message"))
    (puthash (plist-get file :id)
             (expand-file-name (sz/telega--guess-filename msg file) dir)
             sz/telega--save-dir-by-id)
    (or (sz/telega--copy-downloaded-file file)
        (telega-file--download
            file
          :priority 32
          :update-callback #'sz/telega--copy-downloaded-file))))

(defun sz/telega-msg-keys ()
  "Temporarily enable telega message-button bindings."
  (interactive)
  (unless sz/telega-msg-button-map--orig
    (user-error "telega-msg-button-map not captured yet"))
  ;; Make it transient. Note: this alone does not show a menu.
  (set-transient-map sz/telega-msg-button-map--orig t)
  ;; If you use which-key, this shows the popup:
  (when (fboundp 'which-key-show-keymap)
    (which-key-show-keymap "Telega (msg)" sz/telega-msg-button-map--orig)))

(use-package telega
  :ensure nil
  :init
  (setopt telega-use-images t)
  (setopt telega-emoji-use-images nil)
  (setopt telega-emoji-font-family "Noto Color Emoji")

  (setopt telega-online-status-function
          (lambda ()
            ;; Example policy: be "online" only when a telega buffer is visible
            ;; in the selected window.
            (let ((buf (window-buffer (selected-window))))
              (derived-mode-p 'telega-root-mode 'telega-chat-mode))))
  :config
  (unless sz/telega-msg-button-map--orig
    (setq sz/telega-msg-button-map--orig (copy-keymap telega-msg-button-map))
    (define-key telega-chat-mode-map (kbd "M-m") #'sz/telega-msg-keys))

  (sz/keymap-clear! telega-msg-button-map)
  (set-keymap-parent telega-msg-button-map button-map))
