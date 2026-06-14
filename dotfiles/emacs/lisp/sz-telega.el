;; === Telega

(defun sz/keymap-clear! (map)
  "Remove MAP's own bindings in-place, preserving its parent."
  (let ((parent (keymap-parent map)))
    (setcdr map nil)
    (set-keymap-parent map parent)))

(defvar sz/telega-msg-button-map--orig nil
  "Saved copy of telega's original message-button keymap.")

(defun sz/telega-copy-marked-messages-as-org ()
  "Copy marked messages from current telega chat buffer as Org text."
  (interactive)
  (unless (derived-mode-p 'telega-chat-mode)
    (user-error "Not in a telega chat buffer"))
  (unless telega-chatbuf--marked-messages
    (user-error "No marked messages"))

  (let* ((messages (sort (copy-sequence telega-chatbuf--marked-messages)
                         (lambda (a b)
                           (< (plist-get a :id) (plist-get b :id)))))
         (chunks
          (delq nil
                (mapcar
                 (lambda (msg)
                   (let* ((content (plist-get msg :content))
                          (fmt-text (or (plist-get content :text)
                                        (plist-get content :caption)))
                          (text
                           (cond
                            (fmt-text
                             (telega--desurrogate-apply
                              (telega--fmt-text-org (copy-tree fmt-text))))
                            ((telega-msg-content-text msg 'with-voice-note)
                             (substring-no-properties
                              (telega-msg-content-text msg 'with-voice-note))))))
                     (and text
                          (not (string-empty-p text))
                          text)))
                 messages)))
         (output (mapconcat #'identity chunks "\n\n")))
    (unless chunks
      (user-error "Marked messages have no text or captions"))
    (kill-new output)
    (message "Copied %d marked messages as Org" (length chunks))
    output))

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
