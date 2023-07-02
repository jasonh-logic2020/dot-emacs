;;; dot-org --- start org mode

;;; Commentary:

;;; Code:
;;;_ , Org-mode


;; (eval-and-compile
;;   (require 'use-package)
;;   (setq use-package-verbose nil)
;;   (setq use-package-expand-minimally t)
;;   (load "org-settings"))


(message "starting dot-org")


(message "pre length %s" (length load-path))

(setq load-path (cl-remove-if
                 #'(lambda (x)
                     (cl-search "/lisp/org/" x))
                 load-path))

(message "post length %s" (length load-path))

(eval-when-compile
  (setplist 'string-to-multibyte
            (use-package-plist-delete
             (symbol-plist 'string-to-multibyte) 'byte-obsolete-info)))

(setq org-roam-v2-ack t)

(use-package org
  :bind (;; ("M-C"   . jump-to-org-agenda)
         ;; ("C-c o c" . org-capture)
;;; overloaded
         ;; ("M-M"   . org-inline-note)
         ;; ("C-c a" . org-agenda)
         ;; ("C-c C-h" . org-babel-remove-result)
         ("C-c S" . org-store-link)
         ;; ("C-c o l" . org-insert-link)
         )
  :init
  (debug-on-variable-change org-modules)

  ;;agenda  (require 'org-agenda)
  :config
  (defface org-dont-underline-indents '((t :underline nil))
    "Avoid underlining of indentation.")

  (defun org-dont-underline-indents ()
    "Remove underlining at indents."
    (add-to-list 'org-font-lock-extra-keywords
                 '("^[[:space:]]+" 0 'org-dont-underline-indents t) 'append))

  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
  (add-hook 'org-font-lock-set-keywords-hook
            #'org-dont-underline-indents 'append)
  (add-to-list 'auto-insert-alist
               '(("\\.org\\'" . "Org mode")
                 . ["snippet.org" autoinsert-yas-expand]))
  ;; (progn
  ;;   ;; Set up blog for export as an Org-mode project.
  ;;   ;; (setq org-publish-project-alist
  ;;   ;;       '(("blog"
  ;;   ;;          :base-directory "~/.emacs.d/posts/"
  ;;   ;;          :publishing-directory "~/src/blog/_posts/"
  ;;   ;;          :base-extension "org"
  ;;   ;;          :sub-superscript ""
  ;;   ;;          :toc nil
  ;;   ;;          :publishing-function org-html-publish-to-html
  ;;   ;;          :html-extension "html"
  ;;   ;;          :body-only t)))

  (org-display-inline-images t t)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)))

  (defadvice org-babel-execute-src-block (around load-language nil activate)
    "Load language if needed"
    (let ((language (org-element-property :language (org-element-at-point))))
      (unless (cdr (assoc (intern language) org-babel-load-languages))
        (add-to-list 'org-babel-load-languages (cons (intern language) t))
        (org-babel-do-load-languages 'org-babel-load-languages
                                     org-babel-load-languages))
      ad-do-it))

  ;;   ;; (org-babel-do-load-languages
  ;;   ;;  'org-babel-load-languages
  ;;   ;;  '((awk . t)
  ;;   ;;    (C . t)
  ;;   ;;    (calc . t)
  ;;   ;;    (clojure . t)
  ;;   ;;    (css . t)
  ;;   ;;    (gnuplot . t)
  ;;   ;;    (emacs-lisp . t)
  ;;   ;;    (haskell . t)
  ;;   ;;    (java . t)
  ;;   ;;    (js . t)
  ;;   ;;    (latex . t)
  ;;   ;;    (makefile . t)
  ;;   ;;    (org . t)
  ;;   ;;    (perl . t)
  ;;   ;;    (python . t)
  ;;   ;;    (ruby . t)
  ;;   ;;    (scala . t)
  ;;   ;;    (sed . t)
  ;;   ;;    (sh . t)
  ;;   ;;    (shell . t)
  ;;   ;;    (sql . t)
  ;;   ;;    (ditaa . t)))

  ;;   ;;   (setq org-default-notes-file
  ;;   ;;         (concat (expand-file-name "notes.org" projectile-project-name)))

  ;;   (defun org-meta-return-around (org-fun &rest args)
  ;;     "Run `ober-eval-in-repl' if in source code block,
  ;;   `ober-eval-block-in-repl' if at header,
  ;;   and `org-meta-return' otherwise."
  ;;     (if (org-in-block-p '("src"))
  ;;         (let* ((point (point))
  ;;                (element (org-element-at-point))
  ;;                (area (org-src--contents-area element))
  ;;                (beg (copy-marker (nth 0 area))))
  ;;           (if (< point beg)
  ;;               (ober-eval-block-in-repl)
  ;;             (ober-eval-in-repl)))
  ;;       (apply org-fun args)))
  ;;   (advice-add 'org-meta-return :around #'org-meta-return-around)

  ;;   (defadvice org-edit-src-code (around set-buffer-file-name activate compile)
  ;;     (let ((file-name (buffer-file-name)))
  ;;       ad-do-it
  ;;       (setq buffer-file-name file-name)))

  ;;   (setq org-use-fast-todo-selection t)
  ;;   (setq org-treat-S-cursor-todo-selection-as-state-change nil)
  ;;   (setq org-todo-state-tags-triggers
  ;;         (quote (("CANCELLED" ("CANCELLED" . t))
  ;;                 ("WAITING" ("WAITING" . t))
  ;;                 ("HOLD" ("WAITING") ("HOLD" . t))
  ;;                 (done ("WAITING") ("HOLD"))
  ;;                 ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
  ;;                 ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
  ;;                 ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

  ;;   (if (string-match "\\.elc\\'" load-file-name)
  ;;       (add-hook 'after-init-hook
  ;;                 #'(lambda ()
  ;;                     (org-agenda-list)
  ;;                     (org-fit-agenda-window)
  ;;                     (org-resolve-clocks))) t))

  ;; org journal

  (defhydra hydra-org (:color blue :timeout 12 :columns 4)
    "Org commands"
    ("i" (lambda () (interactive) (org-clock-in '(4))) "Clock in")
    ("o" org-clock-out "Clock out")
    ("q" org-clock-cancel "Cancel a clock")
    ("<f10>" org-clock-in-last "Clock in the last task")
    ("j" (lambda () (interactive) (org-clock-goto '(4))) "Go to a clock")
    ("m" make-this-message-into-an-org-todo-item "Flag and capture this message"))
  (global-set-key (kbd "<f10>") 'hydra-org/body)

  (defun get-journal-file-today ()
    "Return filename for today's journal entry."
    (let ((daily-name (format-time-string "%Y%m%d")))
      (expand-file-name (concat org-journal-dir daily-name))))

  (defun journal-file-today ()
    "Create and load a journal file based on today's date."
    (interactive)
    (find-file (get-journal-file-today)))

  (defun get-journal-file-yesterday ()
    "Return filename for yesterday's journal entry."
    (let ((daily-name
           (format-time-string "%Y%m%d"
                               (time-subtract
                                (current-time) (days-to-time 1)))))
      (expand-file-name (concat org-journal-dir daily-name))))

  (defun journal-file-yesterday ()
    "Creates and load a file based on yesterday's date."
    (interactive)
    (find-file (get-journal-file-yesterday)))

  (defun journal-last-year-file ()
    "Returns the string corresponding to the journal entry that
    happened 'last year' at this same time (meaning on the same day
    of the week)."
    (let* ((last-year-seconds (- (float-time) (* 365 24 60 60)))
           (last-year (seconds-to-time last-year-seconds))
           (last-year-dow (nth 6 (decode-time last-year)))
           (this-year-dow (nth 6 (decode-time)))
           (difference (if (> this-year-dow last-year-dow)
                           (- this-year-dow last-year-dow)
                         (- last-year-dow this-year-dow)))
           (target-date-seconds (+ last-year-seconds (* difference 24 60 60)))
           (target-date (seconds-to-time target-date-seconds)))
      (format-time-string "%Y%m%d" target-date)))

  (defun journal-last-year ()
    "Loads last year's journal entry, which is not necessary the
    same day of the month, but will be the same day of the week."
    (interactive)
    (let ((journal-file (concat org-journal-dir (journal-last-year-file))))
      (find-file journal-file)))

  (use-package org-pomodoro
    :disabled t
    :commands org-pomodoro
    :init
    (progn
      (setq org-pomodoro-audio-player "/usr/bin/play")))


  (defun my/remove-empty-drawer-on-clock-out ()
    (interactive)
    (save-excursion
      (beginning-of-line 0)
      (org-remove-empty-drawer-at "LOGBOOK" (point))))

  (add-hook 'org-clock-out-hook 'my/remove-empty-drawer-on-clock-out 'append)

  (defun my/org-insert-link ()
    (interactive)
    (when (org-in-regexp org-bracket-link-regexp 1)
      (goto-char (match-end 0))
      (insert "\n"))
    (call-interactively 'org-insert-link))

  (defadvice org-refile-get-location (before clear-refile-history activate)
    "Fit the Org Agenda to its buffer."
    (setq org-refile-history nil))

  (defun org-get-global-property (name)
    "Get property by NAME in current file."
    (save-excursion
      (goto-char (point-min))
      (and (re-search-forward (concat "#\\+PROPERTY: " name " \\(.*\\)") nil t)
           (match-string 1))))

  (defun org-todo-age-time (&optional pos)
    "Calculate time since CREATED property (at current or POS."
    (let ((stamp (org-entry-get (or pos (point)) "CREATED" t)))
      (when stamp
        (time-subtract (current-time)
                       (org-time-string-to-time
                        (org-entry-get (or pos (point)) "CREATED" t))))))

  (defun org-todo-age (&optional pos)
    "Human-friendly age of item at point (or POS)."
    (let ((days (time-to-number-of-days (org-todo-age-time pos))))
      (cond
       ((< days 1)   "today")
       ((< days 7)   (format "%dd" days))
       ((< days 30)  (format "%.1fw" (/ days 7.0)))
       ((< days 358) (format "%.1fM" (/ days 30.0)))
       (t            (format "%.1fY" (/ days 365.0))))))

  (defun org-compare-todo-age (a b)
    "Compare ages of A and B: -1, 0 (equal), 1."
    (let ((time-a (org-todo-age-time (get-text-property 0 'org-hd-marker a)))
          (time-b (org-todo-age-time (get-text-property 0 'org-hd-marker b))))
      (if (time-less-p time-a time-b)
          -1
        (if (equal time-a time-b)
            0
          1))))

  ;; (defun org-my-message-open (message-id)
  ;;   "MESSAGE-ID."
  ;;   (if (get-buffer "*Group*")
  ;;       (gnus-goto-article
  ;;        (gnus-string-remove-all-properties (substring message-id 2)))
  ;;     (error "Gnus is not running")))

  ;; (add-to-list 'org-link-protocols (list "message" 'org-my-message-open nil))

  (defun save-org-mode-files ()
    "Save all org files."
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (eq major-mode 'org-mode)
          (if (and (buffer-modified-p) (buffer-file-name))
              (save-buffer))))))

  (run-with-idle-timer 25 t 'save-org-mode-files)

  (defun my-org-push-mobile ()
    (interactive)
    (with-current-buffer (find-file-noselect "~/Documents/tasks/todo.org")
      (org-mobile-push)))

  (eval-when-compile
    (defvar org-clock-current-task)
    (defvar org-mobile-directory)
    (defvar org-mobile-capture-file))

  (defun quickping (host)
    (= 0 (call-process "ping" nil nil nil "-c1" "-W50" "-q" host)))

  (defun org-my-auto-exclude-function (tag)
    (and (cond
          ((string= tag "call")
           (let ((hour (nth 2 (decode-time))))
             (or (< hour 8) (> hour 21))))
          ((string= tag "errand")
           (let ((hour (nth 2 (decode-time))))
             (or (< hour 12) (> hour 17))))
          ((or (string= tag "home") (string= tag "nasim"))
           (with-temp-buffer
             (call-process "ifconfig" nil t nil "en0" "inet")
             (call-process "ifconfig" nil t nil "en1" "inet")
             (call-process "ifconfig" nil t nil "bond0" "inet")
             (goto-char (point-min))
             (not (re-search-forward "inet 192\\.168\\.1\\." nil t))))
          ((string= tag "net")
           (not (quickping "imap.fastmail.com")))
          ((string= tag "fun")
           org-clock-current-task))
         (concat "-" tag)))

  (defun my-mobileorg-convert ()
    (interactive)
    (while (re-search-forward "^\\* " nil t)
      (goto-char (match-beginning 0))
      (insert ?*)
      (forward-char 2)
      (insert "TODO ")
      (goto-char (line-beginning-position))
      (forward-line)
      (re-search-forward "^\\[")
      (goto-char (match-beginning 0))
      (let ((uuid
             (save-excursion
               (re-search-forward "^\\*\\* Note ID: \\(.+\\)")
               (prog1
                   (match-string 1)
                 (delete-region (match-beginning 0)
                                (match-end 0))))))
        ;; (insert (format "SCHEDULED: %s\n:PROPERTIES:\n"
        ;;                 (format-time-string (org-time-stamp-format))))
        (insert ":PROPERTIES:\n")
        (insert (format ":ID:       %s\n:CREATED:  " uuid)))
      (forward-line)
      (insert ":END:")))

  (defun my-org-convert-incoming-items ()
    (interactive)
    (with-current-buffer
        (find-file-noselect (expand-file-name org-mobile-capture-file
                                              org-mobile-directory))
      (goto-char (point-min))
      (unless (eobp)
        (my-mobileorg-convert)
        (goto-char (point-max))
        (if (bolp)
            (delete-char -1))
        (let ((tasks (buffer-string)))
          (set-buffer-modified-p nil)
          (kill-buffer (current-buffer))
          (with-current-buffer (find-file-noselect "~/Documents/tasks/todo.org")
            (save-excursion
              (goto-char (point-min))
              (re-search-forward "^\\* Inbox$")
              (re-search-forward "^:END:")
              (forward-line)
              (goto-char (line-beginning-position))
              (if (and tasks (> (length tasks) 0))
                  (insert tasks ?\n))))))))

  (defun my-org-mobile-pre-pull-function ()
    (my-org-convert-incoming-items))

  (add-hook 'org-mobile-pre-pull-hook 'my-org-mobile-pre-pull-function)

  (defun org-my-state-after-clock-out (state)
    (if (string= state "STARTED") "TODO" state))

  (defvar org-my-archive-expiry-days 9
    "The number of days after which a completed task should be auto-archived.
This can be 0 for immediate, or a floating point value.")

  (defconst org-my-ts-regexp
    "[[<]\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [^]>\r\n]*?\\)[]>]"
    "Regular expression for fast inactive time stamp matching.")

  (defun org-my-closing-time ()
    (let* ((state-regexp
            (concat "- State \"\\(?:" (regexp-opt org-done-keywords)
                    "\\)\"\\s-*\\[\\([^]\n]+\\)\\]"))
           (regexp (concat "\\(" state-regexp "\\|" org-my-ts-regexp "\\)"))
           (end (save-excursion
                  (outline-next-heading)
                  (point)))
           begin
           end-time)
      (goto-char (line-beginning-position))
      (while (re-search-forward regexp end t)
        (let ((moment (org-parse-time-string (match-string 1))))
          (if (or (not end-time)
                  (time-less-p (apply #'encode-time end-time)
                               (apply #'encode-time moment)))
              (setq end-time moment))))
      (goto-char end)
      end-time))

  (defun org-archive-expired-tasks ()
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (let ((done-regexp
             (concat "^\\*\\* \\(" (regexp-opt org-done-keywords) "\\) ")))
        (while (re-search-forward done-regexp nil t)
          (if (>= (time-to-number-of-days
                   (time-subtract (current-time)
                                  (apply #'encode-time (org-my-closing-time))))
                  org-my-archive-expiry-days)
              (org-archive-subtree))))
      (save-buffer)))

  (defalias 'archive-expired-tasks 'org-archive-expired-tasks)

  (defun org-archive-done-tasks ()
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\* \\(DONE\\|CANCELED\\) " nil t)
        (if (save-restriction
              (save-excursion
                (org-narrow-to-subtree)
                (search-forward ":LOGBOOK:" nil t)))
            (forward-line)
          (org-archive-subtree)
          (goto-char (line-beginning-position))))))

  (defalias 'archive-done-tasks 'org-archive-done-tasks)

  (defun org-get-inactive-time ()
    (float-time (org-time-string-to-time
                 (or (org-entry-get (point) "TIMESTAMP")
                     (org-entry-get (point) "TIMESTAMP_IA")
                     (debug)))))

  (defun org-get-completed-time ()
    (let ((begin (point)))
      (save-excursion
        (outline-next-heading)
        (and (re-search-backward
              (concat "\\(- State \"\\(DONE\\|DEFERRED\\|CANCELED\\)\""
                      "\\s-+\\[\\(.+?\\)\\]\\|CLOSED: \\[\\(.+?\\)\\]\\)")
              begin t)
             (float-time (org-time-string-to-time (or (match-string 3)
                                                      (match-string 4))))))))

  (defun org-sort-done-tasks ()
    (interactive)
    (goto-char (point-min))
    (org-sort-entries t ?F #'org-get-inactive-time #'<)
    (goto-char (point-min))
    (while (re-search-forward "
+" nil t)
      (delete-region (match-beginning 0) (match-end 0))
      (insert "
"))
    (let (after-save-hook)
      (save-buffer))
    (org-overview))

  (defalias 'sort-done-tasks 'org-sort-done-tasks)

  (defun org-sort-all ()
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\* " nil t)
        (goto-char (match-beginning 0))
        (condition-case err
            (progn
              ;; (org-sort-entries t ?a)
              (org-sort-entries t ?p)
              (org-sort-entries t ?o))
          (error nil))
        (forward-line))
      (goto-char (point-min))
      (while (re-search-forward "\* PROJECT " nil t)
        (goto-char (line-beginning-position))
        (ignore-errors
          ;; (org-sort-entries t ?a)
          (org-sort-entries t ?p)
          (org-sort-entries t ?o))
        (forward-line))))

  (defun org-cleanup ()
    (interactive)
    (org-archive-expired-tasks)
    (org-sort-all))

  (defvar my-org-wrap-region-history nil)

  (defun my-org-wrap-region (&optional arg)
    (interactive "P")
    (save-excursion
      (goto-char (region-end))
      (if arg
          (insert "#+end_src\n")
        (insert ":END:\n"))
      (goto-char (region-beginning))
      (if arg
          (insert "#+begin_src "
                  (read-string "Language: " nil 'my-org-wrap-region-history)
                  ?\n)
        (insert ":OUTPUT:\n"))))

  ;; (defun org-get-message-link (&optional title)
  ;;   (let (message-id subject)
  ;;     (with-current-buffer gnus-original-article-buffer
  ;;       (setq message-id (substring (message-field-value "message-id") 1 -1)
  ;;             subject (or title (message-field-value "subject"))))
  ;;     (org-make-link-string (concat "message://" message-id)
  ;;                           (rfc2047-decode-string subject))))

  (defun org-insert-message-link (&optional arg)
    (interactive "P")
    (insert (org-get-message-link (if arg "writes"))))

  (defun org-set-message-link ()
    "Set a property for the current headline."
    (interactive)
    (org-set-property "Message" (org-get-message-link)))

  (defun org-get-message-sender ()
    (let (message-id subject)
      (with-current-buffer gnus-original-article-buffer
        (message-field-value "from"))))

  (defun org-set-message-sender ()
    "Set a property for the current headline."
    (interactive)
    (org-set-property "Submitter" (org-get-message-sender)))

  (defun org-get-safari-link ()
    (let ((subject (substring (do-applescript
                               (string-to-multibyte "tell application \"Safari\"
        name of document of front window
end tell")) 1 -1))
          (url (substring (do-applescript
                           (string-to-multibyte "tell application \"Safari\"
        URL of document of front window
end tell")) 1 -1)))
      (org-make-link-string url subject)))

  (defun org-get-chrome-link ()
    (let ((subject (do-applescript
                    (string-to-multibyte "tell application \"Google Chrome\"
        title of active tab of front window
end tell")))
          (url (do-applescript
                (string-to-multibyte "tell application \"Google Chrome\"
        URL of active tab of front window
end tell"))))
      (org-make-link-string (substring url 1 -1) (substring subject 1 -1))))

  (defun org-insert-url-link ()
    (interactive)
    (insert (org-get-safari-link)))

  (defun org-set-url-link ()
    "Set a property for the current headline."
    (interactive)
    (org-set-property "URL" (org-get-safari-link)))

  (defun org-get-file-link ()
    (let* ((subject (do-applescript "tell application \"Path Finder\"
     set theItems to the selection
     name of beginning of theItems
end tell"))
           (path (do-applescript "tell application \"Path Finder\"
     set theItems to the selection
     (POSIX path of beginning of theItems) as text
end tell"))
           (short-path
            (replace-regexp-in-string abbreviated-home-dir "~/"
                                      (substring path 1 -1))))
      (org-make-link-string (concat "file:" short-path)
                            (substring subject 1 -1))))

  (defun org-insert-file-link ()
    (interactive)
    (insert (org-get-file-link)))

  (defun org-set-file-link ()
    "Set a property for the current headline."
    (interactive)
    (org-set-property "File" (org-get-file-link)))

  (defun org-set-dtp-link ()
    "Set a property for the current headline."
    (interactive)
    (org-set-property "Document" (org-get-dtp-link)))

  (defun org-dtp-message-open ()
    "Visit the message with the given MESSAGE-ID.
This will use the command `open' with the message URL."
    (interactive)
    (re-search-backward "\\[\\[message://\\(.+?\\)\\]\\[")
    (do-applescript
     (format "tell application \"DEVONthink Pro\"
        set searchResults to search \"%%3C%s%%3E\" within URLs
        open window for record (get beginning of searchResults)
end tell" (match-string 1))))

  (add-hook 'org-log-buffer-setup-hook
            (lambda ()
              (setq fill-column (- fill-column 5))))

  (defun org-message-reply ()
    (interactive)
    (let* ((org-marker (get-text-property (point) 'org-marker))
           (author (org-entry-get (or org-marker (point)) "Author"))
           (subject (if org-marker
                        (with-current-buffer (marker-buffer org-marker)
                          (goto-char org-marker)
                          (nth 4 (org-heading-components)))
                      (nth 4 (org-heading-components)))))
      (setq subject (replace-regexp-in-string "\\`(.*?) " "" subject))
      (compose-mail-other-window author (concat "Re: " subject)))) ;

  ;;Allow hyphenated abbrev names. Unintentional comedy to follow
  (abbrev-table-put org-mode-abbrev-table
                    :regexp "\\_<\\(\\w+\\(-\\w+\\)*\\)?\\W*")

  (defvar repos-build-roots '("~" "/var/shared-elpa/straight/repos"))
  (defvar repos-project-markers '(".git"))

  (defun org-make-repo-url-abbrevs ()
    "Add abbrevs which expand to links for all local repositories."
    (interactive)
    (dolist (f (f-glob "~/*/.git"))
      (let ((e (nth 1 (reverse (f-split f)))))
        (define-abbrev org-mode-abbrev-table
          e (concat "[["
                    (save-match-data
                      (with-temp-buffer
                        (insert-file-contents (concat (file-name-as-directory f)
                                                      "config"))
                        (re-search-forward
                         "^\\W*url\\s-*=\\s-*\\(.*\\)\\.git\\s-")
                        (match-string 1)))
                    "][" e "]]")))))

  (remove-hook 'kill-emacs-hook 'org-babel-remove-temporary-directory)


  (defun org-refile-heading-p ()
    (let ((heading (org-get-heading)))
      (not (string-match "Colophon" heading))))

  (defun org-inline-note ()
    (interactive)
    (switch-to-buffer-other-window "notes.org")
    (goto-char (point-min))
    (forward-line)
    (goto-char (line-beginning-position))
    (insert "* NOTE ")
    (save-excursion
      (insert (format "\n:PROPERTIES:\n:ID:       %s\n:CREATED:  %s\n:END:\n"
                      (substring (shell-command-to-string "uuidgen") 0 -1)
                      (format-time-string (org-time-stamp-format t t)))))
    (save-excursion
      (forward-line)
      (org-cycle)))

  :custom
  (org-src-window-setup 'current-window)
  (org-use-speed-commands t)
  (org-src-fontify-natively t)           ;; better looking source code
  (org-return-follows-link t)            ;; make RET follow links
  (org-list-allow-alphabetical t)        ;; allow alphabetical list
  (org-hide-emphasis-markers t)          ;; hide markers
  (org-pretty-entities t)                ;; make latex look good
  (org-fontify-quote-and-verse-blocks t) ;; make quotes stand out
  (org-table-export-default-format "orgtbl-to-csv") ;; export for
  ;; org-tables to csv
  (org-ellipsis "↷")               ;; nicer elipses "↴" "▼"
  (org-confirm-babel-evaluate nil) ;; evaluate src block without confirmation
  (org-startup-indented t)         ;; start in indent mode
  ;; org-src-tab-acts-natively t ;; indent for src code natively
  ;; org-src-preserve-indentation nil
  ;; org-edit-src-content-indentation t
  (org-imenu-depth 8)
  (org-src-window-setup 'plain)
  (imenu-auto-rescan t)
  (org-plantuml-jar-path (expand-file-name "/usr/share/java/plantuml.jar"))
  (org-M-RET-may-split-line '((headline) (default . t)))
  (org-adapt-indentation nil)
  ;; (org-agenda-auto-exclude-function org-my-auto-exclude-function)
  ;; (org-agenda-cXmp-user-defined org-compare-todo-age)
  ;; (org-agenda-custom-commands
  ;;  '(("h" "Current Hotlist" alltodo ""
  ;;     ((org-agenda-overriding-header "Current Hotlist")
  ;;      (org-agenda-skip-function
  ;;       #'my-org-agenda-skip-all-siblings-but-first-hot))
  ;;     )
  ;;    ("H" "Hot Projects" tags "HOT&TODO=\"PROJECT\""
  ;;     ((org-agenda-overriding-header "Hot Projects")))
  ;;    ("T" "Non-Hot Projects" tags "-HOT&TODO=\"PROJECT\""
  ;;     ((org-agenda-overriding-header "Non-Hot Projects")))
  ;;    ("n" "Project Next Actions" alltodo ""
  ;;     ((org-agenda-overriding-header "Project Next Actions")
  ;;      (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first)))
  ;;    ("P" "All Projects" tags "TODO=\"PROJECT\""
  ;;     ((org-agenda-overriding-header "All Projects")))
  ;;    ("A" "Priority #A tasks" agenda ""
  ;;     ((org-agenda-ndays 1)
  ;;      (org-agenda-overriding-header "Today's priority #A tasks: ")
  ;;      (org-agenda-skip-function
  ;;       '(org-agenda-skip-entry-if 'notregexp "\\=.*\\[#A\\]"))))
  ;;    ("b" "Priority #A and #B tasks" agenda ""
  ;;     ((org-agenda-ndays 1)
  ;;      (org-agenda-overriding-header "Today's priority #A and #B tasks: ")
  ;;      (org-agenda-skip-function
  ;;       (org-agenda-skip-entry-if 'regexp "\\=.*\\[#C\\]"))))
  ;;    ("r" "Uncategorized items" tags "CATEGORY=\"Inbox\"&LEVEL=2"
  ;;     ((org-agenda-overriding-header "Uncategorized items")))
  ;;    ("W" "Waiting/delegated tasks" tags "W-TODO=\"DONE\"|TODO={WAITING\\|DELEGATED}"
  ;;     ((org-agenda-overriding-header "Waiting/delegated tasks:")
  ;;      (org-agenda-skip-function
  ;;       (org-agenda-skip-entry-if 'scheduled))
  ;;      (org-agenda-sorting-strategy
  ;;       (todo-state-up priority-down category-up))))
  ;;    ("D" "Deadlined tasks" tags "TODO<>\"\"&TODO<>{DONE\\|CANCELED\\|NOTE\\|PROJECT}"
  ;;     ((org-agenda-overriding-header "Deadlined tasks: ")
  ;;      (org-agenda-skip-function
  ;;       (org-agenda-skip-entry-if 'notdeadline))
  ;;      (org-agenda-sorting-strategy
  ;;       (category-up))))
  ;;    ("S" "Scheduled tasks" tags "TODO<>\"\"&TODO<>{APPT\\|DONE\\|CANCELED\\|NOTE\\|PROJECT}&STYLE<>\"habit\""
  ;;     ((org-agenda-overriding-header "Scheduled tasks: ")
  ;;      (org-agenda-skip-function
  ;;       (org-agenda-skip-entry-if 'notscheduled))
  ;;      (org-agenda-sorting-strategy
  ;;       (category-up))))
  ;;    ("d" "Unscheduled open source tasks (by date)" tags "TODO<>\"\"&TODO<>{DONE\\|CANCELED\\|NOTE\\|PROJECT}"
  ;;     ((org-agenda-overriding-header "Unscheduled Open Source tasks (by date): ")
  ;;      (org-agenda-skip-function
  ;;       (org-agenda-skip-entry-if 'scheduled 'deadline 'timestamp 'regexp "\\* \\(DEFERRED\\|SOMEDAY\\)"))
  ;;      (org-agenda-sorting-strategy
  ;;       (user-defined-up))
  ;;      (org-agenda-prefix-format "%-11c%5(org-todo-age) ")
  ;;      (org-agenda-files
  ;;       ("~/doc/tasks/OSS.org"))))
  ;;    ("o" "Unscheduled open source tasks (by project)" tags "TODO<>\"\"&TODO<>{DONE\\|CANCELED\\|NOTE\\|PROJECT}"
  ;;     ((org-agenda-overriding-header "Unscheduled Open Source tasks (by project): ")
  ;;      (org-agenda-skip-function
  ;;       (org-agenda-skip-entry-if 'scheduled 'deadline 'timestamp 'regexp "\\* \\(DEFERRED\\|SOMEDAY\\)"))
  ;;      (org-agenda-sorting-strategy
  ;;       (category-up))
  ;;      (org-agenda-prefix-format "%-11c%5(org-todo-age) ")
  ;;      (org-agenda-files
  ;;       ("~/doc/tasks/OSS.org"))))
  ;;    ("u" "Unscheduled tasks" tags "TODO<>\"\"&TODO<>{DONE\\|CANCELED\\|NOTE\\|PROJECT\\|DEFERRED\\|SOMEDAY}"
  ;;     ((org-agenda-overriding-header "Unscheduled tasks: ")
  ;;      (org-agenda-skip-function
  ;;       (org-agenda-skip-entry-if 'scheduled 'deadline 'timestamp))
  ;;      (org-agenda-sorting-strategy
  ;;       (user-defined-up))
  ;;      (org-agenda-prefix-format "%-11c%5(org-todo-age) ")
  ;;      (org-agenda-files
  ;;       ("~/doc/tasks/todo.org" "~/doc/tasks/Bahai.org"))))
  ;;    ("U" "Deferred tasks" tags "TODO=\"DEFERRED\""
  ;;     ((org-agenda-overriding-header "Deferred tasks:")
  ;;      (org-agenda-sorting-strategy
  ;;       (user-defined-up))
  ;;      (org-agenda-prefix-format "%-11c%5(org-todo-age) ")))
  ;;    ("Y" "Someday tasks" tags "TODO=\"SOMEDAY\""
  ;;     ((org-agenda-overriding-header "Someday tasks:")
  ;;      (org-agenda-sorting-strategy
  ;;       (user-defined-up))
  ;;      (org-agenda-prefix-format "%-11c%5(org-todo-age) ")))
  ;;    ("w" "Unscheduled work-related tasks" tags "TODO<>\"\"&TODO<>{DONE\\|DEFERRED\\|CANCELED\\|NOTE\\|PROJECT}"
  ;;     ((org-agenda-overriding-header "Unscheduled work-related tasks")
  ;;      (org-agenda-files
  ;;       ("~/dfinity/docs/dfinity.org"))
  ;;      (org-agenda-sorting-strategy
  ;;       (category-up user-defined-up))
  ;;      (org-agenda-skip-function
  ;;       (org-agenda-skip-entry-if 'scheduled 'deadline 'timestamp))
  ;;      (org-agenda-prefix-format "%-11c%5(org-todo-age) ")))
  ;;    ("c" "Appointment Calendar" agenda ""
  ;;     ((org-agenda-overriding-header "Appointment Calendar")
  ;;      (org-agenda-sorting-strategy
  ;;       (time-up))
  ;;      (org-agenda-span 14)
  ;;      (org-agenda-ndays 14)
  ;;      (org-agenda-regexp-filter-preset
  ;;       ("+APPT"))))))
  ;; (org-agenda-deadline-leaders '("!D!: " "D%02d: "))
  ;; (org-agenda-default-appointment-duration 60)
  ;; (org-agenda-files
  ;;  '("~/doc/tasks/todo.org" "~/doc/tasks/habits.org" "~/dfinity/docs/dfinity.org" "~/doc/tasks/Bahai.org" "~/doc/tasks/OSS.org"))
  ;; (org-agenda-fontify-priorities t)
  ;; (org-agenda-include-diary t)
  ;; (org-agenda-inhibit-startup t)
  ;; (org-agenda-log-mode-items '(closed clock state))
  ;; (org-agenda-ndays 1)
  ;; (org-agenda-persistent-filter t)
  ;; (org-agenda-prefix-format
  ;;  '((agenda . "  %-11c%?-12t% s")
  ;;    (timeline . "  % s")
  ;;    (todo . "  %-11c%5(org-todo-age) ")
  ;;    (tags . "  %-11c")))
  ;; (org-agenda-scheduled-leaders '("" "S%d: "))
  ;; (org-agenda-scheduled-relative-text "S%d: ")
  ;; (org-agenda-scheduled-text "")
  ;; (org-agenda-show-all-dates t)
  ;; (org-agenda-skip-deadline-if-done t)
  ;; (org-agenda-skip-scheduled-if-deadline-is-shown t)
  ;; (org-agenda-skip-scheduled-if-done t)
  ;; (org-agenda-skip-unavailable-files t)
  ;; (org-agenda-sorting-strategy
  ;;  '((agenda habit-down time-up todo-state-up priority-down)
  ;;    (todo priority-down category-keep)
  ;;    (tags priority-down category-keep)
  ;;    (search category-keep)))
  ;; (org-agenda-start-on-weekday nil)
  ;; (org-agenda-tags-column -100)
  ;; (org-agenda-tags-todo-honor-ignore-options t)
  ;; (org-agenda-text-search-extra-files '('agenda-archives
  ;;                                       "~/doc/tasks/notes.org"))
  ;; (org-agenda-todo-ignore-scheduled 'past)
  ;; (org-agenda-use-time-grid nil)
  ;; (org-agenda-window-frame-fractions '(0.5 . 0.75))

  (org-archive-location "TODO-archive::")
  (org-archive-save-context-info '(time category itags))
  (org-attach-file-list-property nil)
  (org-attach-method 'mv)
  (org-attach-store-link-p 'attached)
  (org-author-transforms '(("^Howard Reubenstein$" . "Howard")))
  (org-beamer-frame-default-options "fragile")
  (org-capture-templates
   '(("a" "Add Task" entry
      (file+headline "~/doc/tasks/todo.org" "Inbox")
      "* TODO %?
:PROPERTIES:
:ID:       %(shell-command-to-string \"uuidgen\"):CREATED:  %U
:END:" :prepend t)
     ("n" "Note" entry
      (file "~/doc/tasks/notes.org")
      "* NOTE %?
:PROPERTIES:
:ID:       %(shell-command-to-string \"uuidgen\"):CREATED:  %U
:END:" :prepend t)
     ("c" "Calendar" entry
      (file+headline "~/doc/tasks/todo.org" "Inbox")
      "* APPT %?
:PROPERTIES:
:ID:       %(shell-command-to-string \"uuidgen\"):CREATED:  %U
:END:" :prepend t)
     ("t" "Add Task" entry
      (file+headline "~/doc/tasks/todo.org" "Inbox")
      "* TODO %?
:PROPERTIES:
:ID:       %(shell-command-to-string \"uuidgen\"):CREATED:  %U
:END:" :prepend t)
     ("p" "Protocol" entry
      (file+headline "~/doc/tasks/todo.org" "Inbox")
      "* NOTE %?
#+BEGIN_QUOTE
%i
#+END_QUOTE
:PROPERTIES:
:ID:       %(shell-command-to-string \"uuidgen\"):CREATED:  %U
:URL:      %c
:END:")
     ("L" "Protocol Link" entry
      (file+headline "~/doc/tasks/todo.org" "Inbox")
      "* NOTE %?
[[%:link][%:description]]
#+BEGIN_QUOTE
%i
#+END_QUOTE
:PROPERTIES:
:ID:       %(shell-command-to-string \"uuidgen\"):CREATED:  %U
:URL:      %c
:END:")
     ("j" "Journal entry" entry
      (file+datetree "~/dfinity/docs/dfinity.org")
      "* %?")))
  (org-clock-clocked-in-display nil)
  (org-clock-idle-time 10)
  (org-clock-in-resume t)
  (org-clock-in-switch-to-state "STARTED")
  (org-clock-into-drawer "LOGBOOK")
  (org-clock-mode-line-total 'current)
  (org-clock-out-remove-zero-time-clocks t)
  (org-clock-out-switch-to-state nil)
  (org-clock-persist t)
  (org-clock-persist-file "~/.emacs.d/data/org-clock-save.el")
  (org-clock-resolve-expert t)
  (org-completion-use-ido t)
  (org-confirm-babel-evaluate nil)
  (org-confirm-elisp-link-function nil)
  (org-confirm-shell-link-function nil)
  (org-crypt-disable-auto-save t)
  (org-crypt-key "0xAB37611BDDE48EBD")
  (org-cycle-global-at-bob t)
  (org-deadline-warning-days 14)
  (org-default-notes-file "~/doc/tasks/todo.org")
  (org-depend-tag-blocked nil)
  (org-directory "~/doc/tasks/")
  (org-ditaa-jar-path "~/.nix-profile/lib/ditaa.jar")
  (org-drawers '("PROPERTIES" "CLOCK" "LOGBOOK" "OUT"))
  (org-edit-src-content-indentation 0)
  (org-enforce-todo-dependencies t)
  (org-export-babel-evaluate nil)
  (org-export-latex-classes
   '(("article" "\\documentclass[11pt]{article}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     ("linalg" "\\documentclass{article}
\\usepackage{linalgjh}
[DEFAULT-PACKAGES]
[EXTRA]
[PACKAGES]"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     ("report" "\\documentclass[11pt]{report}"
      ("\\part{%s}" . "\\part*{%s}")
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("book" "\\documentclass[11pt]{book}"
      ("\\part{%s}" . "\\part*{%s}")
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("beamer" "\\documentclass{beamer}" org-beamer-sectioning)))
  (org-extend-today-until 4)
  (org-fast-tag-selection-single-key 'expert)
  (org-fontify-done-headline t)
  (org-fontify-quote-and-verse-blocks t)
  (org-fontify-whole-heading-line t)
  (org-footnote-section nil)
  (org-hide-emphasis-markers t)
  (org-hide-leading-stars t)
  (org-icalendar-combined-agenda-file "~/doc/tasks/org.ics")
  (org-icalendar-timezone "America/Los_Angeles")
  (org-id-locations-file "~/.emacs.d/data/org-id-locations")
  (org-image-actual-width nil)
  (org-imenu-depth 4)
  (org-insert-heading-respect-content t)
  (org-irc-link-to-logs t t)
  (org-latex-default-packages-alist
   '(("T1" "fontenc" t)
     ("" "fixltx2e" nil)
     ("" "graphicx" t)
     ("" "longtable" nil)
     ("" "float" nil)
     ("" "wrapfig" nil)
     ("" "rotating" nil)
     ("normalem" "ulem" t)
     ("" "amsmath" t)
     ("" "textcomp" t)
     ("" "marvosym" t)
     ("" "wasysym" t)
     ("" "amssymb" t)
     ("" "hyperref" nil)
     "\\tolerance=1000"))
  (org-latex-listings 'minted)
  (org-latex-minted-options
   '(("fontsize" "\\footnotesize")
     ("linenos" "true")
     ("xleftmargin" "0em")))
  (org-latex-pdf-process
   '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
     "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
     "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  (org-mime-preserve-breaks nil)
  (org-mobile-directory "~/Dropbox/Apps/MobileOrg")
  (org-mobile-files '("~/doc/tasks/todo.org"))
  (org-mobile-files-exclude-regexp "\\(TODO\\(-.*\\)?\\)\\'")
  (org-mobile-inbox-for-pull "~/doc/tasks/from-mobile.org")
  (org-mode-hook
   (org-babel-result-hide-spec org-babel-hide-all-hashes abbrev-mode))
  (org-modules ())
  (org-pretty-entities t)
  (org-priority-faces
   '((65 :foreground "White" :weight bold)
     (66 . "White")
     (67 :foreground "dark gray" :slant italic)))
  (org-refile-target-verify-function 'org-refile-heading-p)
  (org-refile-targets '((org-agenda-files :todo . "PROJECT")))
  (org-return-follows-link t)
  (org-reverse-note-order t)
  (org-smart-capture-use-lastname t)
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-stuck-projects '("TODO=\"PROJECT\"" ("TODO" "DEFERRED") nil ""))
  (org-subject-transforms
   '(("\\`\\(Re\\|Fwd\\): " . "")
     ("\\`{ledger} " . "")
     ("([Ww]as: .+)\\'" . "")
     ("\\`\\[[a-z-]+\\] " . "")
     ("\\`bug#\\([0-9]+\\):" . "[[x-debbugs-gnu:\\1][#\\1]]")))
  (org-tags-column -97)
  (org-time-clocksum-use-fractional t)
  (org-todo-keyword-faces
   '(("TODO" :foreground "medium blue" :weight bold)
     ("EPIC" :foreground "deep sky blue" :weight bold)
     ("STORY" :foreground "royal blue" :weight bold)
     ("RECUR" :foreground "cornflowerblue" :weight bold)
     ("APPT" :foreground "medium blue" :weight bold)
     ("NOTE" :foreground "brown" :weight bold)
     ("STARTED" :foreground "dark orange" :weight bold)
     ("WAITING" :foreground "red" :weight bold)
     ("DELEGATED" :foreground "dark violet" :weight bold)
     ("DEFERRED" :foreground "dark blue" :weight bold)
     ("SOMEDAY" :foreground "dark blue" :weight bold)
     ("PROJECT" :foreground "#088e8e" :weight bold)))
  (org-todo-repeat-to-state "TODO")
  (org-use-property-inheritance '("AREA"))
  (org-use-speed-commands t)
  (org-use-tag-inheritance nil)
  (org-x-backends '(ox-org ox-redmine))
  (org-x-redmine-title-prefix-function org-x-redmine-title-prefix)
  (org-x-redmine-title-prefix-match-function org-x-redmine-title-prefix-match)
  )

(use-package org-agenda
  :straight nil
  :commands org-agenda-list
  :bind* (("M-C"   . jump-to-org-agenda)
          ("C-c a" . org-agenda))
  :bind (:map
         org-agenda-mode-map
         (" "   . org-agenda-tree-to-indirect-buffer)
         (">"   . org-agenda-filter-by-top-headline)
         ("C-n" . next-line)
         ("C-p" . previous-line)
         ("F"   . org-agenda-follow-mode)
         ("M-m")
         ("M-n" . org-agenda-later)
         ("M-p" . org-agenda-earlier)
         ("b"   . org-agenda-date-earlier)
         ("f"   . org-agenda-date-later)
         ("g"   . org-agenda-redo)
         ("q"   . bury-buffer)
         ("x"   . org-agenda-todo)
         ("w"   . org-agenda-refile)
         ("z"   . pop-window-configuration))
  :custom
  (org-agenda-auto-exclude-function 'org-my-auto-exclude-function)
  (org-agenda-cmp-user-defined 'org-compare-todo-age)
  (org-agenda-compact-blocks t)
  (org-agenda-custom-commands
   '(("h" "Current Hotlist" alltodo ""
      ((org-agenda-overriding-header "Current Hotlist")
       (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first-hot)))
     ("H" "Hot Projects" tags "HOT&TODO=\"PROJECT\""
      ((org-agenda-overriding-header "Hot Projects")))
     ("T" "Non-Hot Projects" tags "-HOT&TODO=\"PROJECT\""
      ((org-agenda-overriding-header "Non-Hot Projects")))
     ("n" "Project Next Actions" alltodo ""
      ((org-agenda-overriding-header "Project Next Actions")
       (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first)))
     ("P" "All Projects" tags "TODO=\"PROJECT\""
      ((org-agenda-overriding-header "All Projects")))
     ("A" "Priority #A tasks" agenda ""
      ((org-agenda-ndays 1)
       (org-agenda-overriding-header "Today's priority #A tasks: ")
       (org-agenda-skip-function
        '(org-agenda-skip-entry-if 'notregexp "\\=.*\\[#A\\]"))))
     ("b" "Priority #A and #B tasks" agenda ""
      ((org-agenda-ndays 1)
       (org-agenda-overriding-header "Today's priority #A and #B tasks: ")
       (org-agenda-skip-function
        '(org-agenda-skip-entry-if 'regexp "\\=.*\\[#C\\]"))))
     ("r" "Uncategorized items" tags "CATEGORY=\"Inbox\"&LEVEL=2"
      ((org-agenda-overriding-header "Uncategorized items")))
     ("W" "Waiting/delegated tasks" tags "W-TODO=\"DONE\"|TODO={WAITING\\|DELEGATED}"
      ((org-agenda-overriding-header "Waiting/delegated tasks:")
       (org-agenda-skip-function
        '(org-agenda-skip-entry-if 'scheduled))
       (org-agenda-sorting-strategy
        '(todo-state-up priority-down category-up))))
     ("D" "Deadlined tasks" tags "TODO<>\"\"&TODO<>{DONE\\|CANCELED\\|NOTE\\|PROJECT}"
      ((org-agenda-overriding-header "Deadlined tasks: ")
       (org-agenda-skip-function
        '(org-agenda-skip-entry-if 'notdeadline))
       (org-agenda-sorting-strategy
        '(category-up))))
     ("S" "Scheduled tasks" tags "TODO<>\"\"&TODO<>{APPT\\|DONE\\|CANCELED\\|NOTE\\|PROJECT}&STYLE<>\"habit\""
      ((org-agenda-overriding-header "Scheduled tasks: ")
       (org-agenda-skip-function
        '(org-agenda-skip-entry-if 'notscheduled))
       (org-agenda-sorting-strategy
        '(category-up))))
     ("o" "Unscheduled open source tasks (by project)" tags "TODO<>\"\"&TODO<>{DONE\\|CANCELED\\|NOTE\\|PROJECT\\|CATEGORY}"
      ((org-agenda-overriding-header "Unscheduled Open Source tasks (by project): ")
       (org-agenda-skip-function
        '(org-agenda-skip-entry-if 'scheduled 'deadline 'timestamp 'regexp "\\* \\(DEFERRED\\|SOMEDAY\\)"))
       (org-agenda-sorting-strategy
        '(category-up))
       (org-agenda-prefix-format "%-11c%5(org-todo-age) ")
       (org-agenda-files
        '("~/doc/org/OSS.org"))))
     ("u" "Unscheduled tasks" tags "TODO<>\"\"&TODO<>{DONE\\|CANCELED\\|NOTE\\|PROJECT\\|CATEGORY\\|DEFERRED\\|SOMEDAY}"
      ((org-agenda-overriding-header "Unscheduled tasks: ")
       (org-agenda-skip-function
        '(org-agenda-skip-entry-if 'scheduled 'deadline 'timestamp))
       (org-agenda-sorting-strategy
        '(user-defined-up))
       (org-agenda-prefix-format "%-11c%5(org-todo-age) ")
       (org-agenda-files
        '("~/doc/org/todo.org"))))
     ("U" "Deferred tasks" tags "TODO=\"DEFERRED\""
      ((org-agenda-overriding-header "Deferred tasks:")
       (org-agenda-sorting-strategy
        '(user-defined-up))
       (org-agenda-prefix-format "%-11c%5(org-todo-age) ")))
     ("Y" "Someday tasks" tags "TODO=\"SOMEDAY\""
      ((org-agenda-overriding-header "Someday tasks:")
       (org-agenda-sorting-strategy
        '(user-defined-up))
       (org-agenda-prefix-format "%-11c%5(org-todo-age) ")))
     ("w" "Unscheduled work-related tasks" tags "TODO<>\"\"&TODO<>{DONE\\|DEFERRED\\|CANCELED\\|NOTE\\|PROJECT\\|CATEGORY}"
      ((org-agenda-overriding-header "Unscheduled work-related tasks")
       (org-agenda-files
        '("~/kadena/docs/kadena.org"))
       (org-agenda-sorting-strategy
        '(category-up user-defined-up))
       (org-agenda-skip-function
        '(org-agenda-skip-entry-if 'scheduled 'deadline 'timestamp))
       (org-agenda-prefix-format "%-11c%5(org-todo-age) ")))
     ("c" "Appointment Calendar" agenda ""
      ((org-agenda-overriding-header "Appointment Calendar")
       (org-agenda-sorting-strategy
        '(time-up))
       (org-agenda-span 14)
       (org-agenda-ndays 14)
       (org-agenda-regexp-filter-preset
        '("+APPT"))))
     ("N" "Notes" tags "TODO=\"NOTE\""
      ((org-agenda-overriding-header "Notes")))))
  (org-agenda-deadline-leaders '("!D!: " "D%02d: " "D-%02d:"))
  (org-agenda-default-appointment-duration 60)
  (org-agenda-files
   '("~/doc/org/todo.org"
     "~/doc/org/habits.org"
     "~/kadena/docs/kadena.org"
     "~/doc/org/OSS.org"))
  (org-agenda-fontify-priorities t)
  (org-agenda-include-diary t)
  (org-agenda-inhibit-startup t)
  (org-agenda-log-mode-items '(closed clock state))
  (org-agenda-ndays 1)
  (org-agenda-persistent-filter t)
  (org-agenda-prefix-format
   '((agenda . "  %-11c%?-12t% s")
     (timeline . "  % s")
     (todo . "  %-11c%5(org-todo-age) ")
     (tags . "  %-11c")))
  (org-agenda-scheduled-leaders '("" "S%d: "))
  (org-agenda-scheduled-relative-text "S%d: ")
  (org-agenda-scheduled-text "")
  (org-agenda-show-all-dates t)
  (org-agenda-show-outline-path nil)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-scheduled-if-deadline-is-shown t)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-unavailable-files t)
  (org-agenda-sorting-strategy
   '((agenda habit-down time-up todo-state-up priority-down)
     (todo priority-down category-keep)
     (tags priority-down category-keep)
     (search category-keep)))
  (org-agenda-span 'day)
  (org-agenda-start-on-weekday nil)
  (org-agenda-tags-column -100)
  (org-agenda-tags-todo-honor-ignore-options t)
  (org-agenda-text-search-extra-files '(agenda-archives))
  (org-agenda-todo-ignore-scheduled 'past)
  (org-agenda-use-time-grid nil)
  (org-agenda-window-frame-fractions '(0.5 . 0.75))
  :custom-face
  (org-agenda-clocking ((t (:background "red2"))))
  (org-agenda-done ((t (:foreground "ForestGreen"))))
  :preface
  (defun org-fit-agenda-window ()
    "Fit the window to the buffer size."
    (and (memq org-agenda-window-setup '(reorganize-frame))
         (fboundp 'fit-window-to-buffer)
         (fit-window-to-buffer)))

  (defun jump-to-org-agenda ()
    (interactive)
    (push-window-configuration)
    (cl-flet ((prep-window
               (wind)
               (with-selected-window wind
                 (org-fit-window-to-buffer wind)
                 (ignore-errors
                   (window-resize
                    wind
                    (- 100 (window-width wind)) t)))))
      (let ((buf (or (get-buffer "*Org Agenda*")
                     (get-buffer "*Org Agenda(a)*"))))
        (if buf
            (let ((win (get-buffer-window buf)))
              (if win
                  (when (called-interactively-p 'any)
                    (funcall #'prep-window win))
                (if (called-interactively-p 'any)
                    (funcall #'prep-window (display-buffer buf t t))
                  (funcall #'prep-window (display-buffer buf)))))
          (call-interactively 'org-agenda-list)
          (funcall #'prep-window (selected-window))))))

  (defun my-org-agenda-should-skip-p ()
    "Skip all but the first non-done entry."
    (let (should-skip-entry)
      (unless (org-current-is-todo)
        (setq should-skip-entry t))
      (when (or (org-get-scheduled-time (point))
                (org-get-deadline-time (point)))
        (setq should-skip-entry t))
      (when (/= (point)
                (save-excursion
                  (org-goto-first-child)
                  (point)))
        (setq should-skip-entry t))
      (save-excursion
        (while (and (not should-skip-entry) (org-goto-sibling t))
          (when (and (org-current-is-todo)
                     (not (org-get-scheduled-time (point)))
                     (not (org-get-deadline-time (point))))
            (setq should-skip-entry t))))
      should-skip-entry))

  (defun my-org-agenda-skip-all-siblings-but-first ()
    "Skip all but the first non-done entry."
    (when (my-org-agenda-should-skip-p)
      (or (outline-next-heading)
          (goto-char (point-max)))))

  (defun my-org-current-tags (depth)
    (save-excursion
      (ignore-errors
        (let (should-skip)
          (while (and (> depth 0)
                      (not should-skip)
                      (prog1
                          (setq depth (1- depth))
                        (not (org-up-element))))
            (if (looking-at "^\*+\\s-+")
                (setq should-skip (org-get-tags))))
          should-skip))))

  (defun my-org-agenda-skip-all-siblings-but-first-hot ()
    "Skip all but the first non-done entry."
    (when (or (my-org-agenda-should-skip-p)
              (not (member "HOT" (my-org-current-tags 1))))
      (or (outline-next-heading)
          (goto-char (point-max)))))

  (defun org-agenda-add-overlays (&optional line)
    "Add overlays found in OVERLAY properties to agenda items.
Note that habitual items are excluded, as they already
extensively use text properties to draw the habits graph.

For example, for work tasks I like to use a subtle, yellow
background color; for tasks involving other people, green; and
for tasks concerning only myself, blue.  This way I know at a
glance how different responsibilities are divided for any given
day.

To achieve this, I have the following in my todo file:

  ,* Work
  :PROPERTIES:
  :CATEGORY: Work
  :OVERLAY:  (face (:background \"#fdfdeb\"))
  :END:
  ,** TODO Task
  ,* Family
  :PROPERTIES:
  :CATEGORY: Personal
  :OVERLAY:  (face (:background \"#e8f9e8\"))
  :END:
  ,** TODO Task
  ,* Personal
  :PROPERTIES:
  :CATEGORY: Personal
  :OVERLAY:  (face (:background \"#e8eff9\"))
  :END:
  ,** TODO Task

The colors (which only work well for white backgrounds) are:

  Yellow: #fdfdeb
  Green:  #e8f9e8
  Blue:   #e8eff9

To use this function, add it to `org-agenda-finalize-hook':

  (add-hook 'org-finalize-agenda-hook 'org-agenda-add-overlays)"
    (let ((inhibit-read-only t)
          (buffer-invisibility-spec '(org-link)))
      (save-excursion
        (goto-char (if line (line-beginning-position) (point-min)))
        (while (not (eobp))
          (let ((org-marker (get-text-property (point) 'org-marker)))
            (when (and org-marker
                       (null (overlays-at (point)))
                       (not (get-text-property (point) 'org-habit-p))
                       (get-text-property (point) 'type)
                       (string-match "\\(sched\\|dead\\|todo\\)"
                                     (get-text-property (point) 'type)))
              (let ((overlays
                     (or (org-entry-get org-marker "OVERLAY" t)
                         (with-current-buffer (marker-buffer org-marker)
                           (org-get-global-property "OVERLAY")))))
                (when overlays
                  (goto-char (line-end-position))
                  (let ((rest (- (window-width) (current-column))))
                    (if (> rest 0)
                        (insert (make-string rest ? ))))
                  (let ((ol (make-overlay (line-beginning-position)
                                          (line-end-position)))
                        (proplist (read overlays)))
                    (while proplist
                      (overlay-put ol (car proplist) (cadr proplist))
                      (setq proplist (cddr proplist))))))))
          (forward-line)))))
  :config
  (add-hook 'org-agenda-finalize-hook 'org-agenda-add-overlays)

  (defadvice org-agenda-redo (after fit-windows-for-agenda-redo activate)
    "Fit the Org Agenda to its buffer."
    (org-fit-agenda-window))

  (defadvice org-agenda (around fit-windows-for-agenda activate)
    "Fit the Org Agenda to its buffer."
    (let ((notes
           (ignore-errors
             (directory-files
              "~/Library/Mobile Documents/iCloud~com~agiletortoise~Drafts5/Documents"
              t "[0-9].*\\.txt\\'" nil))))
      (when notes
        (with-current-buffer (find-file-noselect "~/doc/org/todo.org")
          (save-excursion
            (goto-char (point-min))
            (re-search-forward "^\\* Inbox$")
            (re-search-forward "^:END:")
            (forward-line 1)
            (dolist (note notes)
              (insert
               "** TODO "
               (with-temp-buffer
                 (insert-file-contents note)
                 (goto-char (point-min))
                 (forward-line)
                 (unless (bolp))
                 (insert ?\n)
                 ;; (insert (format "SCHEDULED: %s\n"
                 ;;                 (format-time-string (org-time-stamp-format))))
                 (goto-char (point-max))
                 (unless (bolp)
                   (insert ?\n))
                 (let ((uuid (substring (shell-command-to-string "uuidgen") 0 -1))
                       (file (file-name-nondirectory note)))
                   (string-match
                    (concat "\\`\\([0-9]\\{4\\}\\)"
                            "-\\([0-9]\\{2\\}\\)"
                            "-\\([0-9]\\{2\\}\\)"
                            "-\\([0-9]\\{2\\}\\)"
                            "-\\([0-9]\\{2\\}\\)"
                            "-\\([0-9]\\{2\\}\\)"
                            "\\.txt\\'") file)
                   (let* ((year (string-to-number (match-string 1 file)))
                          (mon (string-to-number (match-string 2 file)))
                          (day (string-to-number (match-string 3 file)))
                          (hour (string-to-number (match-string 4 file)))
                          (min (string-to-number (match-string 5 file)))
                          (sec (string-to-number (match-string 6 file)))
                          (date (format "%04d-%02d-%02d %s"
                                        year mon day
                                        (calendar-day-name (list mon day year) t))))
                     (insert (format (concat ;; "SCHEDULED: <%s>\n"
                                      ":PROPERTIES:\n"
                                      ":ID:       %s\n"
                                      ":CREATED:  ")
                                     uuid))
                     (insert (format "[%s %02d:%02d]\n:END:\n" date hour min))))
                 (buffer-string)))
              (delete-file note t)))
          (when (buffer-modified-p)
            (save-buffer)))))
    ad-do-it
    (org-fit-agenda-window))
  )

(use-package org-modern
  :after org
  :hook ((org-mode . #'org-modern-mode)
         (org-agenda-finalize . #'org-modern-agenda)
         (org-modern-mode . (lambda ()
                              "Adapt `org-modern-mode'."
                              ;; Disable Prettify Symbols mode
                              (setq prettify-symbols-alist nil)
                              (prettify-symbols-mode -1)))))

(use-package org-tag-beautify
  :disabled t
  :after org
  :custom (org-tag-beautify-data-dir (ensure-user-dir "org-tag-beautify"))
  :init (org-tag-beautify-mode +1))

;; ;;;_ , org-projectile

(use-package org-structure-hydra
  :after org hydra
  :straight nil
  :commands (my/org-insert-structure)
  :bind (:map org-mode-map ("<" . 'my/org-insert-structure))
  :preface
  (defhydra hydra-org-template (:color blue :hint nil)
    "
 _c_enter  _q_uote     _e_macs-lisp    _L_aTeX:
 _l_atex   _E_xample   _p_erl          _i_ndex:
 _a_scii   _v_erse     _P_erl tangled  _I_NCLUDE:
 _s_rc     _n_ote      plant_u_ml      _H_TML:
 _h_tml    ^ ^         ^ ^             _A_SCII:
"
    ("s" (hot-expand "<s"))
    ("E" (hot-expand "<e"))
    ("q" (hot-expand "<q"))
    ("v" (hot-expand "<v"))
    ("n" (hot-expand "<not"))
    ("c" (hot-expand "<c"))
    ("l" (hot-expand "<l"))
    ("h" (hot-expand "<h"))
    ("a" (hot-expand "<a"))
    ("L" (hot-expand "<L"))
    ("i" (hot-expand "<i"))
    ("e" (hot-expand "<s" "emacs-lisp"))
    ("p" (hot-expand "<s" "perl"))
    ("u" (hot-expand "<s" "plantuml :file CHANGE.png"))
    ("P" (hot-expand "<s" "perl" ":results output :exports both :shebang \"#!/usr/bin/env perl\"\n"))
    ("I" (hot-expand "<I"))
    ("H" (hot-expand "<H"))
    ("A" (hot-expand "<A"))
    ("<" self-insert-command "ins")
    ("o" nil "quit"))

  (defun hot-expand (str &optional mod header)
    "Expand org template.

STR is a structure template string recognised by org like <s. MOD is a
string with additional parameters to add the begin line of the
structure element. HEADER string includes more parameters that are
prepended to the element after the #+HEADER: tag."
    (let (text)
      (when (region-active-p)
        (setq text (buffer-substring (region-beginning) (region-end)))
        (delete-region (region-beginning) (region-end))
        (deactivate-mark))
      (when header (insert "#+HEADER: " header) (forward-line))
      (insert str)
      (org-try-structure-completion)
      (when mod (insert mod) (forward-line))
      (when text (insert text))))

  (defun my/org-insert-structure ()
    "Insert block when at beginning of line or region active."
    (interactive)
    (if (or (region-active-p) (looking-back "^"))
        (hydra-org-template/body)
      (self-insert-command 1)))

  '(cl-pushnew
    '("not" "#+BEGIN_NOTES\n?\n#+END_NOTES")
    org-structure-template-alist))

(use-package org-bullets
  :unless noninteractive
  :after org
  :commands org-bullets-mode
  :hook (org-mode .
                  (lambda ()
                    (org-bullets-mode +1))))

(use-package org-seek
  :after org
  :commands (org-seek-string org-seek-regexp org-seek-headlines))

(use-package org-autolist
  :after org)

(use-package org-ref
  :disabled t
  :after org counsel-projectile
  :after async
  :config
  (progn
    (setq org-ref-bibliography-notes
          (expand-file-name "notes.org" projectile-project-root)
          org-ref-default-bibliography
          (expand-file-name "references.bib" projectile-project-root)
          org-ref-pdf-directory
          (expand-file-name "bibtex-pdfs/" projectile-project-root))))

(use-package org-board
  :after org projectile
  :config
  (progn
    (setq as/org-board-capture-file
          (expand-file-name "my-board.org" projectile-project-name))
    (defun as/do-org-board-dl-hook ()
      (when (equal (buffer-name)
                   (concat "CAPTURE-" as/org-board-capture-file))
        (org-board-archive)))

    (add-hook 'org-capture-before-finalize-hook
              'as/do-org-board-dl-hook)))

(unless window-system
  (setq org-agenda-files
        '("~/Documents/tasks/todo.org"
          "~/Documents/tasks/Bahai.org"
          "~/Documents/tasks/BAE.org")))

(defconst my-org-soft-red    "#fcebeb")
(defconst my-org-soft-orange "#fcf5eb")
(defconst my-org-soft-yellow "#fcfceb")
(defconst my-org-soft-green  "#e9f9e8")
(defconst my-org-soft-blue   "#e8eff9")
(defconst my-org-soft-purple "#f3e8f9")

(when nil
  (custom-set-faces
   '(variable-pitch ((t (:family "ETBembo")))))
  ;; (custom-set-faces
  ;;  '(org-document-title ((t (:foreground "#171717" :weight bold :height 1.5)))))
  (custom-set-faces
   '(org-document-title ((t (:foreground "#f7f7f7" :weight bold :height 1.5)))))
  ;; (custom-set-faces
  ;;  '(org-done ((t (:background "#E8E8E8" :foreground "#0E0E0E" :strike-through t :weight bold)))))
  ;; (custom-set-faces
  ;;  '(org-headline-done ((t (:foreground "#171717" :strike-through t)))))
  ;; (custom-set-faces
  ;;  '(org-level-1 ((t (:foreground "#090909" :weight bold :height 1.3)))))
  ;; (custom-set-faces
  ;;  '(org-level-2 ((t (:foreground "#090909" :weight normal :height 1.2)))))
  ;; (custom-set-faces
  ;;  '(org-level-3 ((t (:foreground "#090909" :weight normal :height 1.1)))))
  (custom-set-faces
   '(org-image-actual-width '(600)))
  (custom-set-faces
   '(org-block-begin-line ((t (:background "#fbf8ef")))))
  (custom-set-faces
   '(org-block-end-line ((t (:background "#fbf8ef")))))

  (setq default-major-mode 'org-mode)

  (add-hook 'org-mode-hook
            '(lambda ()
               (variable-pitch-mode 1) ;; All fonts with variable pitch.
               (mapc
                (lambda (face) ;; Other fonts with fixed-pitch.
                  (set-face-attribute face nil :inherit 'fixed-pitch))
                (list 'org-code
                      'org-link
                      'org-block
                      'org-table
                      'org-verbatim
                      'org-block-begin-line
                      'org-block-end-line
                      'org-meta-line
                      'org-document-info-keyword)))))

(eval-and-compile
  (require 'cal-julian)
  (require 'diary-lib))

(defun org-current-is-todo ()
  (member (org-get-todo-state) '("TODO" "EPIC" "STORY" "STARTED")))

(defun my-org-agenda-should-skip-p ()
  "Skip all but the first non-done entry."
  (let (should-skip-entry)
    (unless (org-current-is-todo)
      (setq should-skip-entry t))
    (when (or (org-get-scheduled-time (point))
              (org-get-deadline-time (point)))
      (setq should-skip-entry t))
    (when (/= (point)
              (save-excursion
                (org-goto-first-child)
                (point)))
      (setq should-skip-entry t))
    (save-excursion
      (while (and (not should-skip-entry) (org-goto-sibling t))
        (when (and (org-current-is-todo)
                   (not (org-get-scheduled-time (point)))
                   (not (org-get-deadline-time (point))))
          (setq should-skip-entry t))))
    should-skip-entry))

(defun my-org-agenda-skip-all-siblings-but-first ()
  "Skip all but the first non-done entry."
  (when (my-org-agenda-should-skip-p)
    (or (outline-next-heading)
        (goto-char (point-max)))))

(defun my-org-current-tags (depth)
  (save-excursion
    (ignore-errors
      (let (should-skip)
        (while (and (> depth 0)
                    (not should-skip)
                    (prog1
                        (setq depth (1- depth))
                      (not (org-up-element))))
          (if (looking-at "^\*+\\s-+")
              (setq should-skip (org-get-local-tags))))
        should-skip))))

(defun my-org-agenda-skip-all-siblings-but-first-hot ()
  "Skip all but the first non-done entry."
  (when (or (my-org-agenda-should-skip-p)
            (not (member "HOT" (my-org-current-tags 1))))
    (or (outline-next-heading)
        (goto-char (point-max)))))

(use-package anki-editor
  :commands anki-editor-submit)

(use-package calfw
  :bind (("C-c A" . my-calendar)
         :map cfw:calendar-mode-map
         ("M-n" . cfw:navi-next-month-command)
         ("M-p" . cfw:navi-previous-month-command)
         ("j"   . cfw:navi-goto-date-command)
         ("g"   . cfw:refresh-calendar-buffer))
  :commands cfw:open-calendar-buffer
  :functions (cfw:open-calendar-buffer
              cfw:refresh-calendar-buffer
              cfw:org-create-source
              cfw:cal-create-source)
  :preface
  (defun my-calendar ()
    (interactive)
    (let ((buf (get-buffer "*cfw-calendar*"))
          (org-agenda-files
           (cons "~/Documents/tasks/Nasim.org"
                 org-agenda-files)))
      (if buf
          (pop-to-buffer buf nil)
        (cfw:open-calendar-buffer
         :contents-sources
         (list (cfw:org-create-source "Dark Blue")
               (cfw:cal-create-source "Dark Orange"))
         :view 'two-weeks)
        (setq-local org-agenda-files org-agenda-files))))
  :custom
  (cfw:read-date-command
   (lambda nil
     (interactive)
     (let
         ((xs
           (decode-time
            (org-time-string-to-time
             (org-read-date)))))
       (list
        (nth 4 xs)
        (nth 3 xs)
        (nth 5 xs)))))
  :config
  (require 'calfw-cal)
  (use-package calfw-org
    :config
    (setq cfw:org-agenda-schedule-args '(:deadline :timestamp :sexp)))

  (setq cfw:fchar-junction         ?╋
        cfw:fchar-vertical-line    ?┃
        cfw:fchar-horizontal-line  ?━
        cfw:fchar-left-junction    ?┣
        cfw:fchar-right-junction   ?┫
        cfw:fchar-top-junction     ?┯
        cfw:fchar-top-left-corner  ?┏
        cfw:fchar-top-right-corner ?┓))

(use-package ob-diagrams)

(use-package ob-restclient)

(use-package org-babel
  :straight nil
  :no-require
  :after ob-restclient
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)))

  (defadvice org-babel-execute-src-block (around load-language nil activate)
    "Load language if needed"
    (let ((language (org-element-property :language (org-element-at-point))))
      (unless (cdr (assoc (intern language) org-babel-load-languages))
        (add-to-list 'org-babel-load-languages (cons (intern language) t))
        (org-babel-do-load-languages 'org-babel-load-languages
                                     org-babel-load-languages))
      ad-do-it)))

(use-package org-bookmark-heading)

(use-package org-crypt
  :straight nil
  :bind (:map org-mode-map
              ("C-c C-x C-/" . org-decrypt-entry)))

(use-package org-gcal
  :disabled t
  :commands org-gcal-sync
  :custom
  (org-gcal-dir "~/.emacs.d/data/org-gcal/")
  :config
  (setq org-gcal-client-id
        (lookup-password "org-caldav-user.google.com" "jwiegley" 80)
        org-gcal-client-secret
        (lookup-password "org-caldav.google.com" org-gcal-client-id 80)
        org-gcal-file-alist
        '(("jwiegley@gmail.com" .
           "~/Documents/tasks/Google.org")
          ("ajhrtkkubthrda9l40bf95hceo@group.calendar.google.com" .
           "~/Documents/tasks/Bahá'í.org")
          ("57jh2om1vl9sv16sor1mudl030@group.calendar.google.com" .
           "~/Documents/tasks/Family.org")
          ("789ust6872bajeo87oqd2jqfog@group.calendar.google.com" .
           "~/Documents/tasks/Nasim.org")
          ("sacramento.lsa1914@gmail.com" .
           "~/Documents/tasks/Sacramento.org"))))

(use-package org-generate)

(use-package org-mime
  :config
  (add-hook 'message-mode-hook
            (lambda ()
              (local-set-key "\C-c\M-o" 'org-mime-htmlize)))

  (add-hook 'org-mode-hook
            (lambda ()
              (local-set-key "\C-c\M-o" 'org-mime-org-buffer-htmlize)))

  (add-hook 'org-mime-html-hook
            (lambda ()
              (org-mime-change-element-style
               "blockquote" "border-left: 2px solid gray; padding-left: 4px;")
              (org-mime-change-element-style
               "pre" (format "color: %s; background-color: %s; padding: 0.5em;"
                             "#E6E1DC" "#232323")))))

(use-package org-noter
  ;; jww (2020-01-16): This package requires a newer version of Org.
  :disabled t
  :commands org-noter)

(use-package org-opml
  :disabled t)

(use-package org-pdfview
  :config
  (delete '("\\.pdf\\'" . default) org-file-apps)
  (add-to-list 'org-file-apps '("\\.pdf\\'" . org-pdfview-open))
  (add-to-list 'org-file-apps
               '("\\.pdf::\\([[:digit:]]+\\)\\'" . org-pdfview-open)))

(use-package org-protocol
  :straight nil)

(use-package orca
  :after org
  :config
  (setq orca-handler-list
        '((orca-handler-match-url
           "https://emacs.stackexchange.com/"
           "~/Dropbox/org/wiki/emacs.org"
           "\\* Questions")
          (orca-handler-current-buffer
           "\\*\\* Captures")
          (orca-handler-file
           "~/Dropbox/org/ent.org"
           "\\* Articles"))))

(use-package org-indent
  :after org
  :straight nil
  :unless noninteractive
  :hook (org-mode . #'org-indent-mode))

(use-package org-prettify-source-block
  :disabled t  ;; in favor of org-modern
  :straight nil
  :unless noninteractive
  :after org
  :preface
  (defvar-local opsb-org-at-src-begin -1
    "Variable that holds whether last position was a ")

  (defvar opsb-ob-header-symbol ?☰
    "Symbol used for babel headers")

  (defun opsb-org-prettify-src--update ()
    (let ((case-fold-search t)
          (re "^[ \t]*#\\+begin_src[ \t]+[^ \f\t\n\r\v]+[ \t]*")
          found)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward re nil t)
          (goto-char (match-end 0))
          (let ((args (org-trim
                       (buffer-substring-no-properties (point)
                                                       (line-end-position)))))
            (when (org-string-nw-p args)
              (let ((new-cell (cons args opsb-ob-header-symbol)))
                (cl-pushnew new-cell prettify-symbols-alist :test #'equal)
                (cl-pushnew new-cell found :test #'equal)))))

        (setq prettify-symbols-alist
              (cl-set-difference prettify-symbols-alist
                                 (cl-set-difference
                                  (cl-remove-if-not
                                   (lambda (elm)
                                     (eq (cdr elm) opsb-ob-header-symbol))
                                   prettify-symbols-alist)
                                  found :test #'equal)))

        ;; Clean up old font-lock-keywords.
        (font-lock-remove-keywords nil prettify-symbols--keywords)
        (setq prettify-symbols--keywords (prettify-symbols--make-keywords))
        (font-lock-add-keywords nil prettify-symbols--keywords)
        (while (re-search-forward re nil t)
          (font-lock-flush (line-beginning-position) (line-end-position))))))

  (defun opsb-org-prettify-src ()
    "Hide src options via `prettify-symbols-mode'.
      `prettify-symbols-mode' is used because it has
      uncollapsing. It may not be efficient."
    (let* ((case-fold-search t)
           (at-src-block
            (save-excursion
              (beginning-of-line)
              (looking-at "^[ \t]*#\\+begin_src[ \t]+[^ \f\t\n\r\v]+[ \t]*"))))
      ;; Test if we moved out of a block.
      (when (or (and opsb-org-at-src-begin
                     (not at-src-block))
                ;; File was just opened.
                (eq opsb-org-at-src-begin -1))
        (opsb-org-prettify-src--update))
      (setq opsb-org-at-src-begin at-src-block)))

  (defvar opsb-block-alist `(("#+begin_src" . ?╦) ;; ➤ 🖝 ➟ ➤ ✎ ✎
                             ("#+end_src"   . ?╩) ;; □
                             ("#+header:" . ,opsb-ob-header-symbol)
                             ("#+begin_comment" . ?✎)
                             ("#+end_comment" . ?✎)
                             ("#+begin_notes" . ?➤)
                             ("#+end_notes" . ?➤)
                             ("#+begin_quote" . ?»)
                             ("#+end_quote" . ?«)))

  (defsubst opsb-append-upcase (the-list)
    "Duplicate THE-LIST with upcased cars."
    (cl-reduce 'append
               (mapcar (lambda (x) (list x (cons (upcase (car x)) (cdr x))))
                       the-list)))

  (defun opsb-append-org-prettify-symbols ()
    (setq prettify-symbols-alist
          (cl-union prettify-symbols-alist
                    (opsb-append-upcase opsb-block-alist))))

  (defun opsb-delete-org-prettify-symbols ()
    (setq prettify-symbols-alist
          (cl-set-difference prettify-symbols-alist
                             (opsb-append-upcase opsb-block-alist))))

  (define-minor-mode org-prettify-source-block-mode
    "Toggle prettification of org source blocks."
    :lighter ""                         ; for desktop.el
    (if org-prettify-source-block-mode
        (progn
          (turn-on-prettify-symbols-mode)
          (add-hook 'post-command-hook 'opsb-org-prettify-src t t)
          (opsb-append-org-prettify-symbols))
      (remove-hook 'post-command-hook 'opsb-org-prettify-src t)
      (opsb-delete-org-prettify-symbols)))

  :commands (org-prettify-source-block-mode)
  :hook (org-mode . (lambda () (org-prettify-source-block-mode +1))))

(use-package org-habit
  :straight nil
  :after org-agenda
  :custom
  (org-habit-preceding-days 42)
  (org-habit-today-glyph 45)
  :custom-face
  (org-habit-alert-face ((((background light)) (:background "#f5f946"))))
  (org-habit-alert-future-face ((((background light)) (:background "#fafca9"))))
  (org-habit-clear-face ((((background light)) (:background "#8270f9"))))
  (org-habit-clear-future-face ((((background light)) (:background "#d6e4fc"))))
  (org-habit-overdue-face ((((background light)) (:background "#f9372d"))))
  (org-habit-overdue-future-face ((((background light)) (:background "#fc9590"))))
  (org-habit-ready-face ((((background light)) (:background "#4df946"))))
  (org-habit-ready-future-face ((((background light)) (:background "#acfca9")))))

(use-package org-rainbow-tags
  :straight (:host github :repo "KaratasFurkan/org-rainbow-tags")
  :custom
  (org-rainbow-tags-face-attributes
   ;; Default is '(:foreground color :weight 'bold)
   '(:foreground color :inverse-video t :box t :weight 'bold))
  :hook
  (org-mode . org-rainbow-tags-mode))

(use-package org-rich-yank
  :defer 5
  :bind (:map org-mode-map
              ("C-M-y" . org-rich-yank)))

(use-package org-super-agenda
  :preface
  (defun super-jump-to-org-agenda ()
    (interactive)
    (let ((org-super-agenda-groups
           '((:name "Today"
                    :time-grid t
                    :todo "TODAY")
             (:name "Important"
                    :tag "bills"
                    :priority "A")
             (:order-multi
              (2 (:name "Shopping in town"
                        :and (:tag "shopping" :tag "@town"))
                 (:name "Food-related"
                        :tag ("food" "dinner"))
                 (:name "Personal"
                        :habit t
                        :tag "personal")
                 (:name "Space-related (non-moon-or-planet-related)"
                        :and (:regexp ("space" "NASA")
                                      :not (:regexp "moon" :tag "planet")))))
             (:todo "WAITING" :order 8)
             (:todo ("SOMEDAY" "TO-READ" "CHECK" "TO-WATCH" "WATCHING")
                    :order 9)
             (:priority<= "B" :order 1))))
      (org-agenda nil "a")))
  :config
  (org-super-agenda-mode))

(use-package org-treescope
  :custom (org-treescope-query-userbuffer "~/path/to/projects.org")
  :bind (("C-c M-t" . org-treescope)))

(use-package org-velocity
  :bind ("C-, C-." . org-velocity)
  :custom
  '(org-velocity-always-use-bucket t)
  '(org-velocity-bucket "~/doc/tasks/notes.org")
  '(org-velocity-capture-templates
    '(("v" "Velocity" entry
       (file "~/doc/tasks/notes.org")
       "* NOTE %:search
%i%?
:PROPERTIES:
:ID:       %(shell-command-to-string \"uuidgen\"):CREATED:  %U
:END:" :prepend t)))
  (org-velocity-exit-on-match nil)
  (org-velocity-force-new t)
  (org-velocity-search-method 'regexp)
  (org-velocity-use-completion t)
  )

(use-package org-web-tools
  :bind (("C-, C-y" . my-org-insert-url)
         ("C-, C-M-y" . org-web-tools-insert-web-page-as-entry))
  :functions (org-web-tools--org-link-for-url
              org-web-tools--get-first-url)
  :preface
  (declare-function org-web-tools--org-link-for-url "org-web-tools")
  (declare-function org-web-tools--get-first-url "org-web-tools")
  (defun my-org-insert-url (&optional arg)
    (interactive "P")
    (require' org-web-tools)
    (let ((link (org-web-tools--org-link-for-url
                 (org-web-tools--get-first-url))))
      (if arg
          (progn
            (org-set-property "URL" link)
            (message "Added pasteboard link to URL property"))
        (insert link)))))

(use-package orgit
  :disabled t)

(use-package orgnav)

(use-package org-doing
  :after org)

(use-package org-jira
  :after org
  :custom
  (org-jira-working-dir (ensure-user-dir "org-jira/"))
  :config
  )

(use-package ob-async
  :after org
  :defer t)

(use-package ob-http
  :after org
  :defer t)

(use-package ob-kotlin
  :after org
  :defer t)

(use-package ob-go
  :after org
  :defer t)

(use-package ob-sql-mode
  :after org
  :defer t)

(use-package ob-redi
  :disabled t ;; repo vanished
  :after org
  :defer t)

(use-package ob-restclient
  :after org
  :defer t)

(use-package ob-tmux
  :after org
  :defer t
  :custom
  (org-babel-default-header-args:tmux
   '((:results . "silent")   ;
     (:session . "default")   ; The default tmux session to send code to
     (:socket  . nil)            ; The default tmux socket to communicate with
     (:terminal . "gnome-terminal")))
  (org-babel-tmux-session-prefix "ob-")
  (org-babel-tmux-location (executable-find "tmux")))

(use-package org-tree-slide
  :after org
  :custom org-image-actual-width nil)

(use-package org-tree-slide-pauses
  :after org-tree-slide)

(use-package ob-translate
  :after org
  :defer t)

(use-package ox-ioslide
  :after org
  :defer t)

(use-package org-projectile
  :unless noninteractive
  :after org
  :bind (("C-c C-n p" . org-projectile:project-todo-completing-read)
         ("C-c c" . org-capture))
  :config
  (org-projectile-per-project)
  (setq org-projectile-per-project-filepath "notes.org")
  (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
  (setq org-projectile:projects-file
        "~/.emacs.d/org/projects.org")
  (add-to-list 'org-capture-templates
               (org-projectile:project-todo-entry "p")))

(use-package poly-noweb
  :defer t
  :after org)

(use-package poly-org
  :defer t
  :after org)

(use-package org-superstar
  :disabled t
  :hook (org-mode . (lambda () (org-superstar-mode +1))))

(use-package orgtbl-aggregate)

(use-package ox-gfm ;;
  ;; :commands ox-gfm-export-to-markdown
  )

(use-package ox-jira
  :commands ox-jira-export-as-jira)

(use-package git-link
  :bind (("C-c g l" . git-link)
         ("C-c g c" . git-link-commit)
         ("C-c g h" . git-link-homepage)))

(use-package ox-slack
  :commands org-slack-export-to-clipboard-as-slack)

(use-package ox-pandoc
  :disabled t)

(use-package ox-texinfo-plus
  :straight (:host github :repo "tarsius/ox-texinfo-plus")
  :defer t)

(use-package ox-odt
  :disabled t
  :straight (org-mode-ox-odt
             :type git
             :host github
             :repo "kjambunathan/org-mode-ox-odt"
             :branch "master")
  :config
  (add-to-list 'org-export-filter-parse-tree-functions
               (defun org-odt--translate-list-tables (tree backend info)
                 (if (eq backend (or 'markdown
                                     'html))
                     (org-odt--translate-list-tables tree backend info)
                   tree))))

(use-package org-roam
  :disabled t
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory "/path/to/org-files/")
  :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-show-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))))

(use-package yankpad
  :defer 10
  :init
  (setq yankpad-file "~/Documents/tasks/yankpad.org")
  :config
  ;; (bind-key "<f7>" 'yankpad-map)
  ;; (bind-key "<f12>" 'yankpad-expand)
  ;; If you want to complete snippets using company-mode
  ;; (add-to-list 'company-backends #'company-yankpad)
  ;; If you want to expand snippets with hippie-expand
  ;; (add-to-list 'hippie-expand-try-functions-list #'yankpad-expand)
  )

(use-package worf
  :bind (:map org-mode-map
              ("C-c C-j" . worf-goto)))

;; Local Variables:
;;   mode: emacs-lisp
;;   outline-regexp: "^;;;_\\([,. ]+\\)"
;; End:

;;; dot-org.el ends here
