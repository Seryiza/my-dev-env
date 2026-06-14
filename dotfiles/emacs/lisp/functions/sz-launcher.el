;;; universal-launcher.el --- Optimized universal launcher

;;; Commentary:
;; Simplified version that uses the existing Emacs frame

;;; Code:
(when (daemonp)
(require 'recentf)
(recentf-mode 1)
(require 'json)
(require 'url-util)
(require 'calc)

;; Pre-grouped category structure for aesthetic grouping
(defvar universal-launcher--categories
  '((:name "Context" :types (contextual custom-action))
    (:name "Active" :types (buffer running))
    (:name "Tasks" :types (agenda-task))
    (:name "Files & Apps" :types (file app flatpak))
    (:name "Web" :types (bookmark firefox-action))
    (:name "System" :types (command ssh))
    (:name "Tools" :types (emoji calculator kill-ring-item)))
  "Category definitions for the launcher.")

;; Enhanced cache system
(defvar universal-launcher--all-candidates nil "Pre-computed candidates.")
(defvar universal-launcher--last-update 0 "Last time candidates were updated.")
(defvar universal-launcher--update-interval 20 "Update interval in seconds.")
(defvar universal-launcher--previous-frame nil "The previous frame to return to.")

;; Emoji data
(defvar universal-launcher--common-emojis
  '(("Smiling Face" . "😊")
    ("Heart" . "❤️")
    ("Thumbs Up" . "👍")
    ("Thinking Face" . "🤔")
    ("Fire" . "🔥")
    ("Star" . "⭐")
    ("Check Mark" . "✅")
    ("Rocket" . "🚀")
    ("Party Popper" . "🎉")
    ("Eyes" . "👀")
    ("Laughing Face" . "😂")
    ("Clapping Hands" . "👏")
    ("Folded Hands" . "🙏")
    ("Muscle" . "💪")
    ("Sparkles" . "✨")
    ("Warning" . "⚠️")
    ("Information" . "ℹ️")
    ("Question Mark" . "❓")
    ("Prohibited" . "🚫")
    ("Calendar" . "📅")
    ("Clock" . "⏰")
    ("Mail" . "📧")
    ("Lock" . "🔒")
    ("Magnifying Glass" . "🔍")
    ("Light Bulb" . "💡"))
  "Common emojis for quick access.")

(defun universal-launcher--grouped-candidates ()
  "Return candidates grouped by category."
  (let ((candidates '())
        (category-handlers (make-hash-table :test 'eq)))

    ;; Define ALL handlers FIRST - before processing categories
    (puthash 'buffer
             (lambda ()
               (mapcar (lambda (buffer)
                         (cons (format "Buffer: %s"
                                       (buffer-name buffer))
                               (list 'buffer buffer)))
                       (buffer-list)))
             category-handlers)

    (puthash 'running
             (lambda ()
               (mapcar (lambda (app)
                         (cons (format "Running: %s"
                                       (car app))
                               (list 'running (cdr app))))
                       (universal-launcher--get-running-applications)))
             category-handlers)

    (puthash 'file
             (lambda ()
               (mapcar (lambda (file)
                         (let ((filename (file-name-nondirectory file))
                               (directory (file-name-directory file)))
                           (cons (format "File: %s  %s"
                                         filename
                                         (propertize (abbreviate-file-name directory) 'face 'font-lock-comment-face))
                                 (list 'file file))))
                       recentf-list))
             category-handlers)

    (puthash 'app
             (lambda ()
               (mapcar (lambda (app)
                         (cons (format "%s"
                                       (car app))
                               (list 'app (cdr app))))
                       (universal-launcher--get-applications)))
             category-handlers)

    (puthash 'flatpak
             (lambda ()
               (mapcar (lambda (app)
                         (cons (format "Flatpak: %s"
                                       (car app))
                               (list 'app (cdr app))))
                       (universal-launcher--get-flatpak-applications)))
             category-handlers)

    (puthash 'bookmark
             (lambda ()
               (mapcar (lambda (bookmark)
                         (cons (format "Bookmark: %s"
                                       (car bookmark))
                               (list 'bookmark (cdr bookmark))))
                       (universal-launcher--parse-org-bookmarks
                        (expand-file-name "~/org/bookmarks.org"))))
             category-handlers)

    (puthash 'firefox-action
             (lambda ()
               (mapcar (lambda (action)
                         (cons (format "Firefox: %s"
                                       (car action))
                               (list 'firefox-action (cdr action))))
                       (universal-launcher--get-firefox-actions)))
             category-handlers)

    (puthash 'command
             (lambda ()
               (mapcar (lambda (cmd)
                         (cons (format "Command %s"
                                       cmd)
                               (list 'command cmd)))
                       (universal-launcher--get-system-commands)))
             category-handlers)

    (puthash 'emoji
             (lambda ()
               (mapcar (lambda (emoji)
                         (cons (format "Emoji: %s %s"
                                       (car emoji)
                                       (cdr emoji))
                               (list 'emoji (cdr emoji))))
                       universal-launcher--common-emojis))
             category-handlers)

    (puthash 'calculator
             (lambda ()
               (list (cons "Calculator: Enter math expression"
                           (list 'calculator 'ready))))
             category-handlers)

    ;; NEW HANDLERS - Add these BEFORE processing categories
    (puthash 'contextual
             #'universal-launcher--get-contextual-actions
             category-handlers)

    (puthash 'ssh
             #'universal-launcher--get-ssh-hosts
             category-handlers)

    (puthash 'agenda-task
             #'universal-launcher--get-agenda-tasks
             category-handlers)

    (puthash 'kill-ring-item
             #'universal-launcher--get-kill-ring
             category-handlers)

    (puthash 'custom-action
             #'universal-launcher--get-custom-actions
             category-handlers)

    ;; NOW process categories - handlers are all defined above
    (dolist (category universal-launcher--categories)
      (let* ((cat-name (plist-get category :name))
             (types (plist-get category :types))
             (section-items '()))

        (dolist (type types)
          (when-let ((handler (gethash type category-handlers)))
            (setq section-items (append section-items (funcall handler)))))

        (when section-items
          (push (cons (format "%s " cat-name) 'separator) candidates)
          (dolist (item section-items)
            (push (cons (concat "   " (car item)) (cdr item)) candidates)))))

    (nreverse candidates)))

(defun universal-launcher--update-candidates (&optional force)
  "Update cached candidates if needed or FORCE is non-nil."
  (when (or force
            (> (- (float-time) universal-launcher--last-update)
               universal-launcher--update-interval))
    (setq universal-launcher--all-candidates (universal-launcher--grouped-candidates))
    (setq universal-launcher--last-update (float-time))))

(defun universal-launcher--get-running-applications ()
  "Get list of currently running applications."
  (let ((apps '()))
    ;; (with-temp-buffer
    ;;   (when (= 0 (call-process "wmctrl" nil t nil "-l"))
    ;;     (goto-char (point-min))
    ;;     (while (re-search-forward "^\\(0x[0-9a-f]+\\)\\s-+\\S-+\\s-+\\S-+\\s-+\\(.+\\)$" nil t)
    ;;       (let ((window-id (match-string-no-properties 1))
    ;;             (app-name (match-string-no-properties 2)))
    ;;         (unless (string-match-p "\\(Desktop\\|Dock\\|Emacs\\)" app-name)
    ;;           (push (cons app-name (list window-id app-name)) apps))))))
    apps))

(defun universal-launcher--get-applications ()
  "Get list of system applications from .desktop files."
  (let ((apps '())
        (dirs '("/usr/share/applications/"
                "/usr/local/share/applications/"
                "~/.local/share/applications/"
                "/var/lib/flatpak/exports/share/applications/"
                "~/.local/share/flatpak/exports/share/applications/")))
    (dolist (dir dirs)
      (when (file-directory-p (expand-file-name dir))
        (dolist (file (directory-files (expand-file-name dir) t "\\.desktop$"))
          (with-temp-buffer
            (insert-file-contents file)
            (when (re-search-forward "^Name=\\(.+\\)$" nil t)
              (let ((name (match-string 1))
                    exec-line)
                (goto-char (point-min))
                (when (re-search-forward "^Exec=\\(.+\\)$" nil t)
                  (setq exec-line (match-string 1))
                  (push (cons name (replace-regexp-in-string "%[FfUu]" "" exec-line))
                        apps))))))))
    apps))

(defun universal-launcher--get-flatpak-applications ()
  "Get list of installed Flatpak applications."
  (let ((apps '()))
    (when (executable-find "flatpak")
      (with-temp-buffer
        ;; Try both user and system installations
        (dolist (scope '("--user" "--system"))
          (erase-buffer)
          (when (= 0 (call-process "flatpak" nil t nil "list" "--app" scope "--columns=name,application"))
            (goto-char (point-min))
            ;; Skip the header line
            (when (looking-at "Name.*Application ID")
              (forward-line 1))
            (while (not (eobp))
              (let* ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
                     ;; Split on multiple spaces (2 or more) to handle column alignment
                     (parts (split-string line "[ \t]\\{2,\\}" t))
                     (name (when (>= (length parts) 1) (string-trim (nth 0 parts))))
                     (app-id (when (>= (length parts) 2) (string-trim (nth 1 parts)))))
                (when (and name app-id
                           (not (string-empty-p name))
                           (not (string-empty-p app-id))
                           ;; Ensure it looks like a proper app ID
                           (string-match-p "^[a-zA-Z][a-zA-Z0-9._-]*\\.[a-zA-Z][a-zA-Z0-9._-]*" app-id))
                  (push (cons (format "%s (Flatpak)" name)
                              (concat "flatpak run " app-id))
                        apps)))
              (forward-line 1))))))
    ;; Remove duplicates (in case app appears in both user and system)
    (cl-remove-duplicates apps :test (lambda (a b) (string= (cdr a) (cdr b))))))

;; TODO Calculator Module
;; Calculator Module
(defun universal-launcher--is-calculator-input (input)
  "Check if INPUT is a math expression."
  (and (not (string-empty-p input))
       (not (string-match-p "^[[:space:]]*$" input))
       ;; Allow more mathematical symbols and functions
       (string-match-p "^[0-9+\\-*/().,^ %!sincotaqrexplog]+$" input)
       ;; Must contain at least one operator or math function
       (or (string-match-p "[+\\-*/^%]" input)
           (string-match-p "\\(sin\\|cos\\|tan\\|sqrt\\|exp\\|log\\)" input))
       ;; Must contain at least one number
       (string-match-p "[0-9]" input)))

(defun universal-launcher--calculate (expr)
  "Calculate mathematical expression EXPR using calc."
  (condition-case err
      (let* ((clean-expr (string-trim expr))
             ;; Replace common notations
             (calc-expr (replace-regexp-in-string "\\^" "**" clean-expr))
             (calc-expr (replace-regexp-in-string "×" "*" calc-expr))
             (calc-expr (replace-regexp-in-string "÷" "/" calc-expr))
             (result (calc-eval calc-expr)))
        (if (and result
                 (stringp result)
                 (not (string= result ""))
                 (not (string-match-p "\\(Error\\|Bad\\)" result))
                 ;; Accept various number formats including scientific notation
                 (or (string-match-p "^[-+]?[0-9]+\\.?[0-9]*\\(?:[eE][-+]?[0-9]+\\)?$" result)
                     (string-match-p "^[-+]?[0-9]+/[0-9]+$" result))) ; fractions
            result
          nil))
    (error nil)))

(defun universal-launcher--copy-to-clipboard (text)
  "Copy TEXT to system clipboard, handling both X11 and Wayland."
  (cond
   ;; GUI Emacs - use built-in
   ((display-graphic-p)
    (gui-set-selection 'CLIPBOARD text))
   ;; Terminal with wl-copy (Wayland)
   ((executable-find "wl-copy")
    (let ((process (start-process "wl-copy" nil "wl-copy")))
      (process-send-string process text)
      (process-send-eof process)))
   ;; Terminal with xclip (X11)
   ((executable-find "xclip")
    (let ((process (start-process "xclip" nil "xclip" "-selection" "clipboard")))
      (process-send-string process text)
      (process-send-eof process)))
   ;; Fallback
   (t
    (kill-new text)
    (message "Copied to Emacs kill ring (install wl-copy or xclip for system clipboard)"))))

;; Enhanced calculator handler for the main popup function
(defun universal-launcher--handle-calculator-input (input)
  "Handle calculator INPUT with immediate calculation."
  (let ((result (universal-launcher--calculate input)))
    (if result
        (progn
          (universal-launcher--copy-to-clipboard result)
          (message "%s = %s (copied to clipboard)" input result)
          ;; If in a buffer, optionally insert the result
          (when (and universal-launcher--previous-frame
                     (frame-live-p universal-launcher--previous-frame))
            (with-selected-frame universal-launcher--previous-frame
              (when (and (not (minibufferp))
                         (not buffer-read-only)
                         (y-or-n-p "Insert result at point? "))
                (insert result)))))
      (message "Invalid expression: %s" input))))

(defun universal-launcher--get-system-commands ()
  "Get system commands from PATH."
  (let ((commands '()))
    (dolist (dir (parse-colon-path (getenv "PATH")))
      (when (file-directory-p dir)
        (dolist (file (directory-files dir t))
          (when (and (file-executable-p file)
                     (not (file-directory-p file))
                     (not (backup-file-name-p file)))
            (push (file-name-nondirectory file) commands)))))
    (cl-remove-duplicates commands :test #'string=)))

(defun universal-launcher--get-firefox-actions ()
  "Get list of Firefox actions."
  (let ((actions '()))
    (when (= 0 (call-process "pgrep" nil nil nil "-x" "firefox"))
      (push (cons "Focus Firefox window" '(focus-window)) actions)
      (push (cons "Open new tab" '(new-tab)) actions)
      (let ((common-sites '(("Google" . "https://www.google.com")
                            ("GitHub" . "https://github.com")
                            ("YouTube" . "https://www.youtube.com")
                            ("Wikipedia" . "https://en.wikipedia.org"))))
        (dolist (site common-sites)
          (push (cons (concat "Open " (car site))
                      (list 'open-url (cdr site)))
                actions))))
    actions))

(defun universal-launcher--parse-org-bookmarks (file)
  "Parse bookmarks from an org FILE with support for various formats."
  (let ((bookmarks '()))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (org-mode)
        ;; Use org-element-map to parse the entire buffer
        (org-element-map (org-element-parse-buffer) 'link
          (lambda (link)
            (when (member (org-element-property :type link) '("http" "https"))
              (let* ((raw-link (org-element-property :raw-link link))
                     ;; Extract just the URL part using regex, excluding initial [ or ]
                     (url-candidate (if (string-match "^\\(https?://[^]\\[]+\\)" raw-link)
                                        (match-string 1 raw-link)
                                      raw-link))
                     ;; Remove trailing slash if present and it's not the only char after "://"
                     (url (if (and url-candidate
                                   (> (length url-candidate) (if (string-prefix-p "https" url-candidate) 8 7)) ; "https://" is 8, "http://" is 7
                                   (string-suffix-p "/" url-candidate))
                              (substring url-candidate 0 -1)
                            url-candidate))
                     (desc (or (org-element-interpret-data
                                (org-element-contents link))
                               (universal-launcher--extract-domain url))))
                (when url ; Ensure URL is not nil
                  (push (cons (if (string-empty-p desc)
                                  (universal-launcher--extract-domain url)
                                desc)
                              url)
                        bookmarks))))))
        ;; Also parse plain URLs
        (goto-char (point-min))
        ;; Regex now excludes ']', '[', space, tab, and newline from the URL part
        (while (re-search-forward "\\bhttps?://[^]\\[ \t\n]+" nil t)
          (let* ((url-candidate (match-string-no-properties 0))
                 ;; Remove trailing slash if present
                 (url (if (and url-candidate
                               (> (length url-candidate) (if (string-prefix-p "https" url-candidate) 8 7))
                               (string-suffix-p "/" url-candidate))
                          (substring url-candidate 0 -1)
                        url-candidate)))
            (when url ; Ensure URL is not nil
              (unless (rassoc url bookmarks) ; Check against the processed URL
                (push (cons (universal-launcher--extract-domain url) url)
                      bookmarks)))))))
    ;; Sort by description and remove duplicates by URL
    (cl-remove-duplicates
     (sort bookmarks (lambda (a b) (string< (car a) (car b))))
     :test (lambda (a b) (string= (cdr a) (cdr b)))
     :from-end t)))

(defun universal-launcher--extract-domain (url)
  "Extract readable domain name from URL."
  (if (string-match "https?://\\([^/]+\\)" url)
      (let ((domain (match-string 1 url)))
        (if (string-match "^www\\." domain)
            (substring domain 4)
          domain))
    url))

(defun universal-launcher--focus-running-application (app-info)
  "Focus running application using APP-INFO."
  ;; (let ((window-id (car app-info))
  ;;       (app-name (cadr app-info)))
  ;;   ;; (condition-case nil
  ;;   ;;     (call-process "wmctrl" nil nil nil "-i" "-a" window-id)
  ;;   ;;   (error
  ;;   ;;    (call-process "wmctrl" nil nil nil "-a" app-name)))
  ;;   )
  )

(defun universal-launcher--run-application (exec-string)
  "Run application with EXEC-STRING."
  (let* ((exec-parts (split-string exec-string))
         (cmd (car exec-parts))
         (proc (apply #'start-process cmd nil exec-parts)))
    ;; Capture cmd in a closure to avoid void-variable error
    (run-with-timer 0.5 nil
                    (lambda (command-name)
                      ;; (call-process "wmctrl" nil nil nil "-a" command-name)
                      )
                    cmd)))

(defun universal-launcher--handle-firefox-action (action)
  "Handle firefox ACTION."
  (pcase (car action)
    ;; ('focus-window
    ;;  (call-process "wmctrl" nil nil nil "-a" "Firefox"))
    ('new-tab
     (call-process "firefox" nil nil nil "--new-tab" "about:newtab"))
    ('org-task
     (universal-launcher--jump-to-task item))
    ('ssh
     (universal-launcher--ssh-connect item))
    ('kill-ring
     (universal-launcher--yank-from-ring item))
    ('custom-action
     (universal-launcher--execute-custom-action item))
    ('async-shell
     (let ((default-directory (or (locate-dominating-file default-directory ".git")
                                  default-directory)))
       (async-shell-command item)))
    ('open-url
     (let ((url (cadr action)))
       (call-process "firefox" nil nil nil "--new-tab" url)))))

(defun universal-launcher--handle-bookmark (url)
  "Open URL in the default browser."
  (browse-url url))

(defun universal-launcher--run-command (command)
  "Run COMMAND."
  (start-process command nil command))

;; Web search function
(defcustom universal-launcher-default-search-engine "DuckDuckGo"
  "Default search engine for web searches."
  :type 'string
  :group 'universal-launcher)

(defvar universal-launcher--last-search-engine nil
  "Last used search engine.")

(defun universal-launcher--web-search (query)
  "Search the web with QUERY using default browser.
If QUERY looks like a URL, navigate directly to it.
Otherwise, prompt for a search engine."
  (let* ((search-engines
          '(("Google" . "https://www.google.com/search?q=")
            ("Go documentation" . "https://pkg.go.dev/search?q=")
            ("ArchWiki" . "https://wiki.archlinux.org/index.php?search=")
            ("DuckDuckGo" . "https://duckduckgo.com/?q=")
            ("Marginalia" . "https://search.marginalia.nu/search?query=")
            ("Reddit" . "https://www.reddit.com/search/?q=")
            ("Wiby" . "https://wiby.me/?q=")
            ("Anna's Archive" . "https://annas-archive.li/search?q=")
            ("Wikipedia" . "https://en.wikipedia.org/w/index.php?search=")
            ("4get" . "https://4get.ca/web?s=")
            ("Goodreads" . "https://www.goodreads.com/search?q=")
            ("Nix Packages" . "https://search.nixos.org/packages?channel=25.05&show=")
            ("NixOS Options" . "https://search.nixos.org/options?channel=25.05&query=")
            ("DevDocs.io" . "https://devdocs.io/#q=")
            ("Doom discourse" . "https://discourse.doomemacs.org/search?q=")
            ("Doom issues" . "https://github.com/doomemacs/doomemacs/issues?q=")
            ("GitHub" . "https://github.com/search?q=")
            ("Google Images" . "https://www.google.com/search?tbm=isch&q=")
            ("Google Maps" . "https://www.google.com/maps/search/")
            ("Internet Archive" . "https://archive.org/search.php?query=")
            ("Kagi" . "https://kagi.com/search?q=")
            ("MDN" . "https://developer.mozilla.org/en-US/search?q=")
            ("Project Gutenberg" . "https://www.gutenberg.org/ebooks/search/?query=")
            ("Rust Docs" . "https://doc.rust-lang.org/std/?search=")
            ("SourceGraph" . "https://sourcegraph.com/search?q=")
            ("StackOverflow" . "https://stackoverflow.com/search?q=")
            ("Wolfram Alpha" . "https://www.wolframalpha.com/input/?i=")
            ("YouTube" . "https://www.youtube.com/results?search_query=")
            ("Perplexity" . "https://www.perplexity.ai/search/new?q=")
            ("Hacker News" . "https://hn.algolia.com/?q=")
            ("Lobsters" . "https://lobste.rs/search?q=")
            ("arXiv" . "https://arxiv.org/search/?query=")
            ("Semantic Scholar" . "https://www.semanticscholar.org/search?q=")
            ("Google Scholar" . "https://scholar.google.com/scholar?q=")
            ("Go Issues" . "https://github.com/golang/go/issues?q=")
            ("Crates.io" . "https://crates.io/search?q=")
            ("MELPA" . "https://melpa.org/#/?q=")
            ("Man Pages" . "https://man.archlinux.org/search?q=")
            ("Yandex" . "https://yandex.com/search/?text=")
            ("Emacs Docs" . "https://www.gnu.org/software/emacs/manual/html_node/emacs/index.html?search=")
            )))
    ;; Check if query is a URL
    (if (string-match-p "^\\(https?://\\|www\\.\\)" query)
        ;; Navigate directly
        (browse-url (if (string-prefix-p "www." query)
                        (concat "https://" query)
                      query))
      ;; Otherwise, search
      (let* ((default-engine (or universal-launcher--last-search-engine
                                 universal-launcher-default-search-engine
                                 "Google"))
             (engine (completing-read
                      (format "Search with (default %s): " default-engine)
                      (mapcar #'car search-engines)
                      nil t nil nil default-engine))
             (url-base (cdr (assoc engine search-engines)))
             (encoded-query (url-hexify-string query)))
        (setq universal-launcher--last-search-engine engine)
        (browse-url (concat url-base encoded-query))))));; Insert emoji function

(defun universal-launcher--insert-emoji (emoji)
  "Insert EMOJI at point and copy to clipboard."
  (let ((frame universal-launcher--previous-frame))
    (when (and frame (frame-live-p frame))
      (select-frame-set-input-focus frame))
    (gui-set-selection 'CLIPBOARD emoji)
    (message "Emoji '%s' copied to clipboard" emoji)))


;; ============================================================================
;; FRECENCY SYSTEM - The Foundation of Intelligence
;; ============================================================================

(defvar universal-launcher--history-file
  (expand-file-name "universal-launcher-history" user-emacs-directory)
  "File to persist launch history.")

(defvar universal-launcher--launch-history nil
  "Alist of (item . (count . last-time)).")

(defun universal-launcher--load-history ()
  "Load launch history from disk."
  (when (file-exists-p universal-launcher--history-file)
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents universal-launcher--history-file)
          (setq universal-launcher--launch-history (read (current-buffer))))
      (error
       (setq universal-launcher--launch-history nil)
       (message "Warning: Could not load launcher history")))))

(defun universal-launcher--save-history ()
  "Save launch history to disk."
  (condition-case nil
      (with-temp-buffer
        (prin1 universal-launcher--launch-history (current-buffer))
        (write-region (point-min) (point-max) universal-launcher--history-file nil 'silent))
    (error (message "Warning: Could not save launcher history"))))

(defun universal-launcher--record-launch (selection)
  "Record SELECTION in history with frecency scoring."
  (let* ((entry (assoc selection universal-launcher--launch-history))
         (count (if entry (car (cdr entry)) 0))
         (now (float-time)))
    (setf (alist-get selection universal-launcher--launch-history nil nil #'equal)
          (cons (1+ count) now))
    (run-with-idle-timer 1 nil #'universal-launcher--save-history)))

(defun universal-launcher--frecency-score (item-text)
  "Calculate frecency score for ITEM-TEXT.
Combines frequency (usage count) with recency (time decay)."
  (if-let ((data (alist-get item-text universal-launcher--launch-history nil nil #'equal)))
      (let* ((count (car data))
             (last-time (cdr data))
             (age-days (/ (- (float-time) last-time) 86400.0))
             ;; Exponential decay: half-life of 7 days
             (recency-factor (exp (/ (- age-days) 7.0))))
        (* count recency-factor))
    0))

;; ============================================================================
;; CONTEXTUAL ACTIONS - Mode-Aware Intelligence
;; ============================================================================

(defun universal-launcher--get-contextual-actions ()
  "Get actions relevant to current buffer's major mode and project."
  (when (and universal-launcher--previous-frame
             (frame-live-p universal-launcher--previous-frame))
    (with-selected-frame universal-launcher--previous-frame
      (let ((actions '()))

        ;; Universal org-capture (always available)
        (when (fboundp 'org-capture)
          (push (cons "Capture: Quick note"
                      (list 'function #'org-capture))
                actions))

        ;; Mode-specific actions
        (pcase major-mode
          ;; Org Mode
          ('org-mode
           (when (fboundp 'org-agenda)
             (push (cons "Org: Open Agenda"
                         (list 'function #'org-agenda))
                   actions))
           (push (cons "Org: Refile"
                       (list 'function #'org-refile))
                 actions)
           (push (cons "Org: Archive subtree"
                       (list 'function #'org-archive-subtree))
                 actions)
           (when (fboundp 'org-set-tags-command)
             (push (cons "Org: Set tags"
                         (list 'function #'org-set-tags-command))
                   actions)))

          ;; Go Mode
          ('go-mode
           (let ((default-directory (or (locate-dominating-file default-directory "go.mod")
                                        default-directory)))
             (push (cons "Go: Run tests"
                         (list 'async-shell "go test -v ./..."))
                   actions)
             (push (cons "Go: Build"
                         (list 'async-shell "go build"))
                   actions)
             (push (cons "Go: Run main"
                         (list 'async-shell "go run ."))
                   actions)
             (push (cons "Go: Tidy modules"
                         (list 'async-shell "go mod tidy"))
                   actions)
             (push (cons "Go: Format code"
                         (list 'function #'gofmt))
                   actions)))

          ;; Rust Mode
          ('rust-mode
           (let ((default-directory (or (locate-dominating-file default-directory "Cargo.toml")
                                        default-directory)))
             (push (cons "Rust: Build"
                         (list 'async-shell "cargo build"))
                   actions)
             (push (cons "Rust: Run tests"
                         (list 'async-shell "cargo test"))
                   actions)
             (push (cons "Rust: Run"
                         (list 'async-shell "cargo run"))
                   actions)
             (push (cons "Rust: Check"
                         (list 'async-shell "cargo check"))
                   actions)))

          ;; Emacs Lisp Mode
          ('emacs-lisp-mode
           (push (cons "Elisp: Eval buffer"
                       (list 'function #'eval-buffer))
                 actions)
           (push (cons "Elisp: Eval defun"
                       (list 'function #'eval-defun))
                 actions)
           (push (cons "Elisp: Load file"
                       (list 'function #'load-file))
                 actions))

          ;; Nix Mode
          ('nix-mode
           (push (cons "Nix: Rebuild switch"
                       (list 'async-shell "sudo nixos-rebuild switch"))
                 actions)
           (push (cons "Nix: Rebuild test"
                       (list 'async-shell "sudo nixos-rebuild test"))
                 actions)
           (push (cons "Nix: Update flake"
                       (list 'async-shell "nix flake update"))
                 actions))

          ;; Markdown Mode
          ('markdown-mode
           (when (fboundp 'markdown-preview)
             (push (cons "Markdown: Preview"
                         (list 'function #'markdown-preview))
                   actions))
           (push (cons "Markdown: Export to HTML"
                       (list 'function #'markdown-export))
                 actions)))

        (nreverse actions)))))

;; ============================================================================
;; SSH HOST LAUNCHER
;; ============================================================================

(defun universal-launcher--get-ssh-hosts ()
  "Get SSH hosts from ~/.ssh/config."
  (let ((hosts '())
        (config (expand-file-name "~/.ssh/config")))
    (when (file-exists-p config)
      (with-temp-buffer
        (insert-file-contents config)
        (goto-char (point-min))
        (while (re-search-forward "^Host \\(.+\\)$" nil t)
          (let ((host (string-trim (match-string 1))))
            ;; Skip wildcards and comments
            (unless (or (string-match-p "[*?]" host)
                        (string-prefix-p "#" host))
              (push (cons (format "SSH: %s" host)
                          (list 'ssh host))
                    hosts))))))
    (nreverse hosts)))

(defun universal-launcher--ssh-connect (host)
  "Connect to SSH HOST using best available terminal."
  (cond
   ;; Prefer vterm if available
   ((fboundp 'vterm)
    (let* ((buffer-name (format "*ssh-%s*" host))
           (existing-buffer (get-buffer buffer-name)))
      (if existing-buffer
          (progn
            (switch-to-buffer existing-buffer)
            (delete-other-windows))
        ;; Create new vterm and send SSH command
        (let ((buf (vterm buffer-name)))
          (with-current-buffer buf
            (vterm-send-string (format "ssh %s" host))
            (vterm-send-return))
          (switch-to-buffer buf)
          (delete-other-windows)))))

   ;; Fallback to eshell
   ((fboundp 'eshell)
    (let ((buffer (generate-new-buffer (format "*ssh-%s*" host))))
      (switch-to-buffer buffer)
      (delete-other-windows)
      (eshell-mode)
      (insert (format "ssh %s" host))
      (eshell-send-input)))

   ;; Last resort: shell-mode
   (t
    (let ((buffer (get-buffer-create (format "*ssh-%s*" host))))
      (switch-to-buffer buffer)
      (delete-other-windows)
      (unless (comint-check-proc buffer)
        (shell buffer))
      (goto-char (point-max))
      (insert (format "ssh %s" host))
      (comint-send-input)))))

;; ============================================================================
;; ORG AGENDA INTEGRATION
;; ============================================================================

(defun universal-launcher--get-agenda-tasks ()
  "Get today's agenda tasks."
  (when (and (fboundp 'org-map-entries)
             (bound-and-true-p org-agenda-files))
    (let ((tasks '()))
      (org-map-entries
       (lambda ()
         (let* ((heading (org-get-heading t t t t))
                (todo-state (org-get-todo-state))
                (priority (org-get-priority (thing-at-point 'line t)))
                (tags (org-get-tags))
                (display (format "%s %s%s"
                                 (propertize (or todo-state "TODO")
                                             'face 'org-todo)
                                 heading
                                 (if tags
                                     (propertize (format " :%s:" (string-join tags ":"))
                                                 'face 'org-tag)
                                   ""))))
           (push (cons display
                       (list 'org-task (point-marker)))
                 tasks)))
       "+TODO=\"TODO\"|+TODO=\"NEXT\"|+TODO=\"STARTED\""
       'agenda)
      (nreverse tasks))))

(defun universal-launcher--jump-to-task (marker)
  "Jump to org task at MARKER."
  (when (marker-buffer marker)
    (switch-to-buffer (marker-buffer marker))
    (goto-char marker)
    (org-show-context)
    (org-reveal)
    (recenter)))

;; ============================================================================
;; KILL RING SEARCH
;; ============================================================================

(defun universal-launcher--get-kill-ring ()
  "Get recent kill ring entries."
  (cl-loop for item in (seq-take kill-ring 15)
           for idx from 1
           when (and (stringp item)
                     (> (length item) 0)
                     (not (string-match-p "^[[:space:]]*$" item)))
           collect (cons (format "Clip #%d: %s"
                                 idx
                                 (truncate-string-to-width
                                  (replace-regexp-in-string "\n" "↩ " item)
                                  60 nil nil "…"))
                         (list 'kill-ring item))))

(defun universal-launcher--yank-from-ring (text)
  "Insert TEXT from kill ring at point."
  (when (and universal-launcher--previous-frame
             (frame-live-p universal-launcher--previous-frame))
    (with-selected-frame universal-launcher--previous-frame
      (when (not buffer-read-only)
        (insert text)
        (message "Inserted from kill ring")))))

;; ============================================================================
;; CUSTOM ACTIONS/SCRIPTS
;; ============================================================================

(defcustom universal-launcher-custom-actions nil
  "Custom actions as ((name . (type . action))).
Type can be 'function, 'shell-command, or 'async-shell.

Examples:
  ((\"Daily Review\" . (function . my-daily-review-fn))
   (\"Rebuild NixOS\" . (async-shell . \"sudo nixos-rebuild switch\"))
   (\"Git Status\" . (shell-command . \"git status\")))"
  :type '(alist :key-type string
          :value-type (cons symbol sexp))
  :group 'universal-launcher)

(defun universal-launcher--get-custom-actions ()
  "Get user-defined custom actions."
  (mapcar (lambda (action)
            (cons (format "Custom: %s" (car action))
                  (list 'custom-action (cdr action))))
          universal-launcher-custom-actions))

(defun universal-launcher--execute-custom-action (action)
  "Execute custom ACTION."
  (let ((type (car action))
        (cmd (cdr action)))
    (pcase type
      ('function
       (if (functionp cmd)
           (funcall cmd)
         (message "Error: Not a valid function: %s" cmd)))
      ('shell-command
       (shell-command cmd))
      ('async-shell
       (async-shell-command cmd))
      (_
       (message "Unknown action type: %s" type)))))

(defun universal-launcher-popup ()
  "World-class launcher for Emacs."
  (interactive)

  ;; Store current frame
  (setq universal-launcher--previous-frame (selected-frame))

  ;; Force update if needed
  (universal-launcher--update-candidates)

  ;; Create candidates list with nil as completion table to allow any input
  (let* ((candidates (mapcar #'car universal-launcher--all-candidates))
         (prompt "Launch (or enter math expression): ")
         (selection
          (minibuffer-with-setup-hook
              (lambda ()
                ;; Allow any input, not just candidates
                (setq-local completion-styles '(substring partial-completion basic))
                (setq-local completion-category-overrides nil))
            (completing-read prompt
                             ;; Use a function that always returns all candidates
                             ;; This allows typing anything while still showing candidates
                             (lambda (string pred action)
                               (if (eq action 'metadata)
                                   '(metadata (category . universal-launcher))
                                 (all-completions string candidates pred)))
                             nil    ; predicate
                             nil    ; require-match = nil allows any input!
                             nil    ; initial-input
                             nil    ; hist
                             nil))) ; def
         (candidate (cdr (assoc selection universal-launcher--all-candidates))))

    (cond
     ;; Empty input - do nothing
     ((string-empty-p selection) nil)

     ;; Calculator check - prioritize this before other matches
     ((universal-launcher--is-calculator-input selection)
      (universal-launcher--handle-calculator-input selection))

     ;; Separator - do nothing
     ((eq candidate 'separator) nil)

     ;; Handle matched candidates
     (candidate
      (let ((type (car candidate))
            (item (cadr candidate)))
        (pcase type
          ('buffer (switch-to-buffer item))
          ('running (universal-launcher--focus-running-application item))
          ('app (universal-launcher--run-application item))
          ('firefox-action (universal-launcher--handle-firefox-action item))
          ('bookmark (universal-launcher--handle-bookmark item))
          ('file (find-file item))
          ('command (universal-launcher--run-command item))
          ('emoji (universal-launcher--insert-emoji item))
          ('ssh (universal-launcher--ssh-connect item))
          ('calculator (message "Type a math expression like: 2+2, sqrt(16), sin(45)"))
          ('org-task (universal-launcher--jump-to-task item))
          ('kill-ring (universal-launcher--yank-from-ring item))
          ('custom-action (universal-launcher--execute-custom-action item))
          ('function (funcall item))
          ('async-shell
           (let ((default-directory (or (locate-dominating-file default-directory ".git")
                                        default-directory)))
             (async-shell-command item)))
          ('shell-command
           (let ((default-directory (or (locate-dominating-file default-directory ".git")
                                        default-directory)))
             (shell-command item)))
          (_ (message "Unknown action type: %s" type)))))

     ;; Web search fallback - only if not a calculator expression
     ((and (not candidate)
           (not (string-empty-p selection))
           (not (universal-launcher--is-calculator-input selection)))
      (universal-launcher--web-search selection)))

    ;; Return to previous frame
    (when (and universal-launcher--previous-frame
               (frame-live-p universal-launcher--previous-frame))
      (select-frame-set-input-focus universal-launcher--previous-frame))))

;; Set up background update timer
(run-with-timer universal-launcher--update-interval
                universal-launcher--update-interval
                #'universal-launcher--update-candidates)
)

(provide 'universal-launcher)
;;; universal-launcher.el ends here
