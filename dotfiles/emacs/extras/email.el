;;; Emacs Bedrock
;;;
;;; Extra config: Email

;;; Usage: Append or require this file from init.el for Email in Emacs. You will
;;; need to do some heavy customization depending on your email provider.

;;; Contents:
;;;
;;;  - Core Email Packages
;;;  - Sample Setup: Gmail
;;;  - Sample Setup: Fastmail

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Core Email Packages
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Sample Setup: Gmail
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Sample Setup: Fastmail
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package mu4e
  :ensure nil                                  ;; mu4e comes with mu
  :commands mu4e
  :config
  (setq mail-user-agent 'mu4e-user-agent)
  (setq mu4e-update-interval 300      ; seconds
      mu4e-index-update-in-background t)
  (setq mu4e-maildir (expand-file-name "~/Maildir") ;; your Maildir root
        mu4e-get-mail-command "offlineimap" ;; or offlineimap/getmail
        ;; mu4e-update-interval 300                      ;; seconds; nil to disable
        message-send-mail-function 'smtpmail-send-it)) ;; uses built-in smtpmail
