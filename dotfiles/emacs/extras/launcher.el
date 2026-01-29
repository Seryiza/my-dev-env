;; (require 'org-element)
;; (require 'seq)

;; (defun browser-bookmarks (org-file)
;;   "Return all links from ORG-FILE."
;;   (with-temp-buffer
;;     (let (links)
;;       (insert-file-contents org-file)
;;       (org-mode)
;;       (org-element-map (org-element-parse-buffer) 'link
;;         (lambda (link)
;;           (let* ((raw-link (org-element-property :raw-link link))
;;                  (content (org-element-contents link))
;;                  (title (substring-no-properties (or (seq-first content) raw-link))))
;;             (push (concat title
;;                           "\n"
;;                           (propertize raw-link 'face 'whitespace-space)
;;                           "\n")
;;                   links)))
;;         nil nil 'link)
;;       (seq-sort 'string-greaterp links))))

;; (defmacro present (&rest body)
;;   "Create a buffer with BUFFER-NAME and eval BODY in a basic frame."
;;   (declare (indent 1) (debug t))
;;   `(let* ((buffer (get-buffer-create (generate-new-buffer-name "*present*")))
;;           (frame (make-frame '((auto-raise . t)
;; 			       (name . "*present*")
;;                                (font . "Iosevka 15")
;;                                (top . 200)
;;                                (height . 20)
;;                                (width . 110)
;;                                (internal-border-width . 20)
;;                                (left . 0.33)
;;                                (left-fringe . 0)
;;                                (line-spacing . 3)
;;                                (menu-bar-lines . 0)
;;                                (minibuffer . only)
;;                                (right-fringe . 0)
;;                                (tool-bar-lines . 0)
;;                                (undecorated . t)
;;                                (unsplittable . t)
;;                                (vertical-scroll-bars . nil)))))
;;      (set-face-attribute 'ivy-current-match frame
;;                          :background "#2a2a2a"
;;                          :foreground 'unspecified)
;;      (select-frame frame)
;;      (select-frame-set-input-focus frame)
;;      (with-current-buffer buffer
;;        (condition-case nil
;;            (unwind-protect
;;                ,@body
;;              (delete-frame frame)
;;              (kill-buffer buffer))
;;          (quit (delete-frame frame)
;;                (kill-buffer buffer))))))

;; (defun present-open-bookmark-frame ()
;;   (present (browse-url (seq-elt (split-string (completing-read "Open: " (browser-bookmarks "~/org/rss.org")) "\n") 1))))

;;; sway-app-launcher.el -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)

(defgroup sway-app-launcher nil
  "Simple app launcher (Rofi-like) using Emacs completion + .desktop files."
  :group 'external
  :prefix "sway-app-launcher-")

(defcustom sway-app-launcher-launch-method 'start-process
  "How to launch apps.

- start-process: run argv directly (preferred; avoids shell quoting issues).
- swaymsg: call `swaymsg exec -- <cmdline>` (useful if you rely on sway env/exec semantics).
- shell: `start-process-shell-command` with the Exec line (most compatible, least safe)."
  :type '(choice (const :tag "start-process" start-process)
                 (const :tag "swaymsg exec" swaymsg)
                 (const :tag "shell" shell)))

(defcustom sway-app-launcher-update-interval 30
  "Seconds between rescans of application entries."
  :type 'integer)

(defcustom sway-app-launcher-frame-parameters
  '((name . "sway-app-launcher")
    (title . "sway-app-launcher")
    (minibuffer . only)
    (width . 90)
    (height . 12)
    (undecorated . t)
    (skip-taskbar . t)
    (no-other-frame . t)
    (internal-border-width . 18))
  "Frame parameters used for the transient launcher frame."
  :type 'alist)

(defcustom sway-app-launcher-application-dirs nil
  "Optional explicit list of application dirs containing .desktop files.
If nil, derive from XDG + common NixOS locations."
  :type '(repeat directory))

(defcustom sway-app-launcher-include-nodisplay nil
  "Whether to include NoDisplay=true and Hidden=true desktop entries."
  :type 'boolean)

(defvar sway-app-launcher--cache nil)        ; list of app plists
(defvar sway-app-launcher--cache-time 0)    ; float-time

(defun sway-app-launcher--string-bool (s)
  (when s
    (member (downcase (string-trim s)) '("1" "true" "yes"))))

(defun sway-app-launcher--xdg-application-dirs ()
  "Return a de-duped list of XDG application dirs (best-effort, NixOS-friendly)."
  (let* ((xdg-data-home (or (getenv "XDG_DATA_HOME")
                            (expand-file-name "~/.local/share")))
         (xdg-data-dirs (split-string (or (getenv "XDG_DATA_DIRS")
                                          "/usr/local/share:/usr/share")
                                      ":" t))
         (nixos-ish (delq nil (list (expand-file-name "~/.nix-profile/share")
                                   (let ((u (user-login-name)))
                                     (when u
                                       (expand-file-name (format "/etc/profiles/per-user/%s/share" u))))
                                   "/run/current-system/sw/share")))
         (bases (append (list xdg-data-home) xdg-data-dirs nixos-ish))
         (apps (mapcar (lambda (b) (expand-file-name "applications" b)) bases)))
    (cl-delete-duplicates
     (cl-remove-if-not #'file-directory-p apps)
     :test #'string=)))

(defun sway-app-launcher--application-dirs ()
  (or sway-app-launcher-application-dirs
      (sway-app-launcher--xdg-application-dirs)))

(defun sway-app-launcher--desktop-parse (file)
  "Parse FILE (a .desktop) and return an alist of keys in [Desktop Entry]."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((in-entry nil)
          (acc '()))
      (while (not (eobp))
        (let ((line (string-trim (buffer-substring-no-properties
                                  (line-beginning-position)
                                  (line-end-position)))))
          (cond
           ((or (string-empty-p line)
                (string-prefix-p "#" line)
                (string-prefix-p ";" line))
            nil)
           ((and (string-prefix-p "[" line) (string-suffix-p "]" line))
            (setq in-entry (string= line "[Desktop Entry]")))
           ((and in-entry (string-match "\\`\\([^=]+\\)=\\(.*\\)\\'" line))
            (let* ((k (string-trim (match-string 1 line)))
                   (v (match-string 2 line)))
              (push (cons k v) acc)))))
        (forward-line 1))
      acc)))

(defun sway-app-launcher--desktop-get (alist key)
  (cdr (assoc key alist)))

(defun sway-app-launcher--clean-exec (exec desktop-file)
  "Remove field codes like %U, %f, etc. Replace %k with desktop-file path."
  (let* ((s (or exec "")))
    ;; Replace %k (desktop file path)
    (setq s (replace-regexp-in-string "%k" desktop-file s t t))
    ;; Unescape %%
    (setq s (replace-regexp-in-string "%%" "%" s t t))
    ;; Drop other field codes (%u, %U, %f, %F, %i, %c, etc.)
    (setq s (replace-regexp-in-string "%[a-zA-Z]" "" s t))
    ;; Cleanup whitespace
    (string-trim (replace-regexp-in-string "[ \t\n]+" " " s))))

(defun sway-app-launcher--exec->argv (exec)
  "Turn an Exec string into argv; best-effort."
  (let ((argv (split-string-and-unquote (or exec ""))))
    ;; Handle leading VAR=VAL assignments (rare but possible)
    (let ((envs '()))
      (while (and argv (string-match-p "\\`[A-Za-z_][A-Za-z0-9_]*=" (car argv)))
        (push (car argv) envs)
        (setq argv (cdr argv)))
      (when envs
        (setq argv (append (list "env") (nreverse envs) argv))))
    argv))

(defun sway-app-launcher--valid-app-p (app)
  (and (plist-get app :name)
       (plist-get app :exec)
       (not (string-empty-p (plist-get app :name)))
       (not (string-empty-p (plist-get app :exec)))))

(defun sway-app-launcher--collect-apps ()
  "Scan .desktop files and return a list of app plists."
  (let ((apps '())
        (dirs (sway-app-launcher--application-dirs)))
    (dolist (dir dirs)
      (dolist (file (directory-files dir t "\\.desktop\\'" t))
        (condition-case _
            (let* ((data (sway-app-launcher--desktop-parse file))
                   (type (sway-app-launcher--desktop-get data "Type"))
                   (name (or (sway-app-launcher--desktop-get data "Name")
                             (file-name-base file)))
                   (exec (sway-app-launcher--desktop-get data "Exec"))
                   (hidden (sway-app-launcher--string-bool
                            (sway-app-launcher--desktop-get data "Hidden")))
                   (nodisplay (sway-app-launcher--string-bool
                               (sway-app-launcher--desktop-get data "NoDisplay")))
                   (tryexec (sway-app-launcher--desktop-get data "TryExec")))
              (when (and (or (null type) (string= type "Application"))
                         (or sway-app-launcher-include-nodisplay
                             (not hidden))
                         (or sway-app-launcher-include-nodisplay
                             (not nodisplay)))
                (when (or (null tryexec) (executable-find tryexec))
                  (let* ((clean-exec (sway-app-launcher--clean-exec exec file))
                         (id (file-name-base file))
                         (display (format "%s  (%s)" name id)))
                    (push (list :id id :name name :display display
                                :exec clean-exec :desktop file)
                          apps)))))
          (error nil))))
    ;; De-dupe by display string (cheap and usually enough)
    (cl-delete-duplicates apps :test (lambda (a b)
                                       (string= (plist-get a :display)
                                                (plist-get b :display))))))

(defun sway-app-launcher--ensure-cache ()
  (when (or (null sway-app-launcher--cache)
            (> (- (float-time) sway-app-launcher--cache-time)
               sway-app-launcher-update-interval))
    (setq sway-app-launcher--cache (sway-app-launcher--collect-apps))
    (setq sway-app-launcher--cache-time (float-time))))

(defun sway-app-launcher--launch (app)
  (let* ((exec (plist-get app :exec))
         (argv (sway-app-launcher--exec->argv exec)))
    (pcase sway-app-launcher-launch-method
      ('start-process
       (when (and argv (car argv))
         (apply #'start-process (car argv) nil argv)))
      ('swaymsg
       (let ((cmdline (mapconcat #'shell-quote-argument argv " ")))
         (start-process "swaymsg" nil "swaymsg" "exec" "--" cmdline)))
      ('shell
       (start-process-shell-command "app-launch" nil exec)))))

(defun sway-app-launcher--in-transient-frame (thunk)
  "Create a transient launcher frame, run THUNK, then delete the frame."
  (let* ((origin (selected-frame))
         (frame (make-frame sway-app-launcher-frame-parameters)))
    (unwind-protect
    (progn
      (select-frame-set-input-focus frame)
      (funcall thunk))
    (when (frame-live-p frame) (delete-frame frame)))))

;;;###autoload
(defun sway-app-launcher ()
  "Prompt for an application (from .desktop entries) and launch it."
  (interactive)
     (sway-app-launcher--ensure-cache)
  (sway-app-launcher--in-transient-frame
   (lambda ()
     (let* ((cands (mapcar (lambda (a) (cons (plist-get a :display) a))
                           sway-app-launcher--cache))
            (choice (completing-read "Launch: " cands nil t)))
       (when-let ((app (cdr (assoc choice cands))))
         (when (sway-app-launcher--valid-app-p app)
	   (let ((inhibit-message t))

	     (sway-app-launcher--launch app))
           )))))
  )
