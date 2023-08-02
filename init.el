;;; init.el -- set up running environment

;;; Commentary:
;; Sadly, my opus.

;;;_.  Initialization

;; This assumes common-emacs-directory has been created. The steps are:

;; $ sudo mkdir -p /var/shared-elpa/elpa/gnupg
;; $ sudo groupadd sharedelpa

;; $ sudo chgrp -R sharedelpa /var/shared-elpa
;; $ sudo chmod -R 2775 /var/shared-elpa
;; $ sudo chmod 775 /var/shared-elpa/elpa/gnupg

;; and then either singly:
;; $ sudo usermod -a -G sharedelpa $username

;; or for all interactive users:

;; sudo getent passwd | while IFS=: read -r name password uid gid gecos home shell; do if [ -d "$home" ] && [ "$(stat -c %u "$home")" = "$uid" ]; then if [ ! -z "$shell" ] && [ "$shell" != "/bin/false" ] && [ "$shell" != "/sbin/nologin" ] && [ $name != 'root' ]; then usermod -a -G sharedelpa $name  ; fi; fi; done

;; n.b.: ALL logged-in users will have to close their sessions and log in
;; again for the changes to take effect. In fact, reboot. It is not enough
;; for one user to log out and back in.

;;; Code:

;; using straight instead of package
;; (defconst package-archives
;;   '(("melpa-stable" . "https://stable.melpa.org/packages/")
;;     ("melpa" . "https://melpa.org/packages/")
;;     ("elpa" . "https://elpa.gnu.org/packages/")))

;; (defvar package-check-signature nil)

;; (require 'cl-lib)
;; (defvar install-run nil)
;; (when install-run
;;   (package-initialize))
;; ;; best guess single-user setup
;; (defconst user-emacs-directory "~/.Emacs.d/")
;; (defconst common-elpa-directory user-emacs-directory)
;; (defconst common-emacs-directory user-emacs-directory)

(defun report-time-since-load (&optional suffix)
  (message "Loading init...done (%.3fs)%s"
           (float-time (time-subtract (current-time) emacs-start-time))
           suffix))

(add-hook 'after-init-hook
          #'(lambda () (report-time-since-load " [after-init]"))
          t)

(eval-and-compile
  (defsubst emacs-path (path)
    (expand-file-name path user-emacs-directory)))

(eval-when-compile
  (dolist (sym '(cl-flet lisp-complete-symbol))
    (setplist sym (use-package-plist-delete
                   (symbol-plist sym) 'byte-obsolete-info))))

;; (defconst user-data-directory
;;   (emacs-path (if emacs-data-suffix
;;                   (format "data-%s" emacs-data-suffix)
;;                 "data")))

(defun user-data (dir)
  (expand-file-name dir user-emacs-directory))

(defconst user-number (- (user-uid) 1000))

(add-hook 'after-init-hook #'garbage-collect t)

(defvar dot-gnus)
(defvar org-settings)

(setq custom-file (convert-standard-filename
                   (expand-file-name "settings.el" user-emacs-directory))
      dot-gnus (convert-standard-filename
                (expand-file-name "dot-gnus.el" user-emacs-directory))
      org-settings (convert-standard-filename
                    (expand-file-name "org-settings.el" user-emacs-directory)))

(set-frame-font "Deja Vu Sans Mono 8" nil t)

(use-package modus-themes
  :init
  (progn
    (setq modus-themes-bold-constructs t)
    (setq modus-themes-org-blocks 'greyscale)
    (setq modus-themes-italic-constructs t)

    (setq modus-themes-headings
          '((1 . (1.6))
            (2 . (background 1.5))
            (3 . (background bold 1.2))
            (4 . (1.1))
            (t . ())))

    (load-theme 'modus-vivendi t)))

(eval-and-compile

  (defvar saved-window-configuration nil)

  (defun push-window-configuration ()
    (interactive)
    (push (current-window-configuration) saved-window-configuration))

  (defun pop-window-configuration ()
    (interactive)
    (let ((config (pop saved-window-configuration)))
      (if config
          (set-window-configuration config)
        (if (> (length (window-list)) 1)
            (delete-window)
          (bury-buffer)))))

  (defun ensure-directory (maybe-dir)
    "Make MAYBE-DIR and its parents if it doesn't exist."
    (unless (file-exists-p maybe-dir)
      (make-directory maybe-dir :parents))
    (convert-standard-filename (file-name-as-directory maybe-dir)))

  (defsubst ensure-user-dir (dir)
    "Ensure user-emacs-directory/DIR exists."
    (ensure-directory (locate-user-emacs-file dir)))

  (defsubst user-file-path (dir file)
    "Return user-emacs-directory/DIR/FILE, ensuring DIR exists."
    (convert-standard-filename (expand-file-name file (ensure-user-dir dir))))

  (defun maybe-load-user-file (filename)
    "Load FILENAME relative to ㅌuser-emacs-directoryㅌ."
    (let ((user-file (locate-user-emacs-file filename)))
      (if (file-exists-p user-file)
          (progn
            (load-file user-file)
            user-file)
        (progn
          (message "File %s not found" user-file)
          nil))))

  (defun filter (f args)
    (let (result)
      (dolist (arg args)
        (when (funcall f arg)
          (setq result (cons arg result))))
      (nreverse result)))

  (setq package-user-dir (ensure-user-dir "elpa")
        custom-file (convert-standard-filename
                     (expand-file-name "settings.el" user-emacs-directory)))

  (defun catdir (root &rest dirs)
    (apply 'concat (mapcar
                    (lambda (name) (file-name-as-directory name))
                    (push root dirs))))
  (setq load-path
        (append
         (let ((package-list '()))
           (dolist (lib (cl-remove-if
                         (lambda (x) (or (equal x ".")
                                    (equal x "..")))
                         (directory-files package-user-dir)))
             (push
              (convert-standard-filename
               (catdir package-user-dir lib))
              package-list))
           package-list)
         (delete-dups load-path)

         (list (ensure-user-dir "lisp"))))

  (defun lookup-password (host user port)
    (require 'auth-source)
    (let ((auth (auth-source-search :host host :user user :port port)))
      (if auth
          (let ((secretf (plist-get (car auth) :secret)))
            (if secretf
                (funcall secretf)
              (error "Auth entry for %s@%s:%s has no secret!"
                     user host port)))
        (error "No auth entry found for %s@%s:%s" user host port))))

  ;; (unless (require 'use-package nil t)
  ;;   (progn
  ;;     (message "No use-package found, auto-installing")
  ;;     ;; update the package metadata if the local cache is missing
  ;;     (unless package-archive-contents
  ;;       (package-refresh-contents))

  ;;     (package-install 'use-package)
  ;;     (require 'use-package)))

  (defconst load-path-reject-re "/\\.emacs\\.d/\\(lib\\|site-lisp\\)/")
  "Regexp matching `:load-path' values to be rejected."

  (defun load-path-handler-override (orig-func name keyword args rest state)
    (if (cl-some (apply-partially #'string-match load-path-reject-re) args)
        (use-package-process-keywords name rest state)
      (let ((body (use-package-process-keywords name rest state)))
        (use-package-concat
         (mapcar #'(lambda (path)
                     `(eval-and-compile (add-to-list 'load-path ,path t)))
                 args)
         body))))
  
  (advice-add 'use-package-handler/:load-path
              :around #'load-path-handler-override))

(defun hoagie-rename-and-select-occur-buffer ()
  "Renames the current buffer to *Occur: [term] [buffer]*.
Meant to be added to `occur-hook'."
  (destructuring-bind (search-term _ (buffer-name &rest _))
      occur-revert-arguments
    (pop-to-buffer
     (rename-buffer (format "*Occur: %s %s*" search-term buffer-name) t))))
(add-hook 'occur-hook #'hoagie-rename-and-select-occur-buffer)

;; (when install-run
;;   (setq use-package-always-ensure t))

;; (unless noninteractive
;;   (message "Loading %s..." load-file-name))

;; (use-package diminish                     :demand t)
;; (use-package bind-key)

;;;_ , use-package extensions

;; (use-package quelpa-use-package
;;   :defer t)

(use-package el-patch)

(use-package use-package-chords
  :config (key-chord-mode 1))

;; spacial placement for use-package extension
(use-package major-mode-hydra
  :bind
  ("M-SPC" . major-mode-hydra))

;; (use-package use-package-secret
;;   :disabled t
;;   :load-path "/home/emacs/use-package-secret"
;;   :custom
;;   (use-package-secret-verbose t))

(add-to-list 'process-coding-system-alist
             '("python" . (utf-8 . utf-8))
             '("bash" . (undecided-dos . undecided-unix)))

;;;_ , no-littering

(use-package no-littering
  :custom
  (no-littering-etc-directory
   (ensure-user-dir "config/"))
  (no-littering-var-directory
   (ensure-user-dir "data/")))

;;;_ , system-wide modifications
;; (require 'dired-x)
;; (require 'epa)
;; (require 'time)
;; (require 'epa-file)

;; (epa-file-enable)

(use-package epg
  :defer t
  :custom
  (epg-pinentry-mode   'loopback)      ; ask in emacs
  )

(setq-default
 line-spacing                   1       ; prevent :box redraws
 indent-tabs-mode             nil       ; Use spaces for indentation
 major-mode             'org-mode) ; Org-mode as default mode

(use-package emacs
  :straight nil
  :bind* ("<C-return>" . other-window)
  :custom
  ;; C source code
  (auto-hscroll-mode            'current-line)
  (auto-save-interval           64)
  (auto-save-timeout            2)
  (enable-recursive-minibuffers t)
  (fill-column                  78)
  (history-delete-duplicates    t)
  (history-length               200)
  (load-prefer-newer            t)
  (menu-bar-mode                nil)
  (message-log-max              16384)
  (redisplay-dont-pause         t)
  (tool-bar-mode                nil)
  (undo-limit                   800000)
  (user-full-name               (or (getenv "USERNAME")
                                    (getenv "USER")))
  (visible-bell                 t)
  (x-stretch-cursor             t)

  (frame-title-format
   '(:eval
     (concat
      (if buffer-file-name default-directory "%b")
      "    "
      (number-to-string
       (cdr
        (assq 'width
              (frame-parameters))))
      "x"
      (number-to-string
       (cdr
        (assq 'height
              (frame-parameters)))))))

  (completion-ignored-extensions
   '(".a"
     ".aux"
     ".bbl"
     ".bin"
     ".blg"
     ".class"
     ".cp"
     ".cps"
     ".elc"
     ".fmt"
     ".fn"
     ".fns"
     ".git/"
     ".glo"
     ".glob"
     ".gmo"
     ".hg/"
     ".idx"
     ".ky"
     ".kys"
     ".la"
     ".lib"
     ".ln"
     ".lo"
     ".lof"
     ".lot"
     ".mem"
     ".mo"
     ".o"
     ".pg"
     ".pgs"
     ".pyc"
     ".pyo"
     ".so"
     ".tfm"
     ".toc"
     ".tp"
     ".tps"
     ".v.d"
     ".vio"
     ".vo" ".vok" ".vos"
     ".vr"
     ".vrs"
     "~"))

  ;; startup.el
  (auto-save-list-file-prefix (user-data "auto-save-list/.saves-"))
  (inhibit-startup-echo-area-message t)
  (inhibit-startup-screen t)
  (initial-buffer-choice t)
  (initial-major-mode 'fundamental-mode)
  (initial-scratch-message "")
  (user-mail-address "johnw@newartisans.com")

  ;; advice.el
  (ad-redefinition-action 'accept)

  ;; files.el
  (auto-save-file-name-transforms '(("\\`/[^/]*:.*" "/tmp" t)))
  (backup-directory-alist '(("." . "~/.local/share/emacs/backups")))
  (delete-old-versions t)
  (directory-abbrev-alist
   '(("\\`/org" . "/Users/johnw/doc/org")))
  (directory-free-space-args "-kh")
  (large-file-warning-threshold nil)
  (save-abbrevs 'silently)
  (trash-directory "~/.Trash")
  (version-control t)

  ;; simple.el
  (backward-delete-char-untabify-method 'untabify)
  (column-number-mode t)
  (indent-tabs-mode nil)
  (kill-do-not-save-duplicates t)
  (kill-ring-max 500)
  (kill-whole-line t)
  (line-number-mode t)
  (mail-user-agent 'gnus-user-agent)
  (next-line-add-newlines nil)
  (save-interprogram-paste-before-kill t)

  ;; bytecomp.el
  (byte-compile-verbose nil)

  ;; (custom-buffer-done-function 'kill-buffer)
  ;; (default-major-mode 'text-mode)

  ;; prog-mode.el
  (prettify-symbols-unprettify-at-point 'right-edge)

  ;; scroll-bar.el
  (scroll-bar-mode nil)

  ;; paragraphs.el
  (sentence-end-double-space nil)

  ;; paren.el
  (show-paren-delay 0)

  ;; window.el
  (same-window-buffer-names
   '("*eshell*"
     "*shell*"
     "*mail*"
     "*inferior-lisp*"
     "*ielm*"
     "*scheme*"))
  (switch-to-buffer-preserve-window-point t)

  ;; warnings.el
  (warning-minimum-log-level :error)

  ;; frame.el
  (window-divider-default-bottom-width 1)
  (window-divider-default-places 'bottom-only)

  ;; nsm.el
  (nsm-settings-file (user-data "network-security.data"))

  :custom-face
  ;; (cursor ((t (:background "hotpink"))))
  ;; (highlight ((t (:background "blue4"))))
  ;; (minibuffer-prompt ((t (:foreground "grey80"))))
  ;; (mode-line-inactive ((t (:background "grey50"))))
  ;; (nobreak-space ((t nil)))
  ;; (variable-pitch ((t (:height 1.2 :family "Bookerly"))))

  :init
  (setq disabled-command-function nil) ;; enable all commands

  :config
  (add-hook 'after-save-hook
            #'executable-make-buffer-file-executable-if-script-p)

  (define-key input-decode-map [?\C-m] [C-m])

  ;; Setup keymaps that are bound into by many declarations below.

  (eval-and-compile
    (mapc #'(lambda (entry)
              (define-prefix-command (cdr entry))
              (bind-key (car entry) (cdr entry)))
          '(("C-,"   . my-ctrl-comma-map)
            ("<C-m>" . my-ctrl-m-map)
            ("C-h e" . my-ctrl-h-e-map)
            ("C-h x" . my-ctrl-h-x-map)
            ("C-c b" . my-ctrl-c-b-map)
            ("C-c e" . my-ctrl-c-e-map)
            ("C-c i" . my-ctrl-c-i-map)
            ("C-c m" . my-ctrl-c-m-map)
            ("C-c n" . my-ctrl-c-n-map)
            ("C-c t" . my-ctrl-c-t-map)
            ("C-c w" . my-ctrl-c-w-map)
            ("C-c y" . my-ctrl-c-y-map)
            ("C-c H" . my-ctrl-c-H-map)
            ("C-c N" . my-ctrl-c-N-map)
            ("C-c (" . my-ctrl-c-open-paren-map)
            ("C-c -" . my-ctrl-c-minus-map)
            ("C-c =" . my-ctrl-c-equals-map)
            ("C-c ." . my-ctrl-c-dot-map)))))

(setq
 echo-keystrokes              0.5 ; minibuffer echo delay (default 1 sec)
 auth-sources '("~/.authinfo.gpg" "~/.authinfo") ; set auth store
 auth-source-debug              t ; always debug due to constant decay
 initial-major-mode     'org-mode ; orgmode please
 scroll-step                    1 ; How many lines to scroll at once
 scroll-conservatively      10000 ; Max lines to scroll to recenter point
 scroll-preserve-screen-position t ; Max lines to scroll to recenter point
 search-whitespace-regexp   ".*?" ; regex for whitespace in search term
 auto-window-vscroll            t ; Adjust scroll for tall glyphs
 auto-revert-verbose          nil ; Be quiet about reverts
 disabled-command-function    nil ; Enable disabled commands
 display-time-24hr-format       t ; 24 hour time format
 eshell-hist-ignoredups         t ; Ignore duplicate history
 eshell-history-size         1000 ; Lengthen Eshell history
 inhibit-startup-screen         t ; No startup screen
 inhibit-startup-message        t ; No startup message
 password-cache-expiry        nil ; Cache TRAMP passwords forever
 sentence-end-double-space    nil
 save-place-file "~/.emacs.d/saved-point-places"
 delete-old-versions            t      ; Delete without asking
 kept-new-versions              6      ; Number of versions to keep
 kept-old-versions              2      ; Number of old versions
 version-control                t      ; Keep versions of every file
 confirm-kill-processes       nil      ; kill processes without asking
 show-paren-delay               0      ; Don't delay the paren update
 enable-recursive-minibuffers   t      ; Enable minibuffer recursion
 minibuffer-prompt-properties    '(read-only t
                                        ; don’t allow the cursor
                                        ; in the minibuffer prompt
                                             cursor-intangible-mode t
                                             face minibuffer-prompt)
 native-comp-async-report-warnings-errors nil)

(auto-insert-mode               1) ; Insert templates in new files
(column-number-mode             1) ; Show column number
(electric-quote-mode            1) ; Easier “quote” typing
(fset 'yes-or-no-p      'y-or-n-p) ; Make "yes/no" prompts "y/n"
(global-auto-revert-mode       +1) ; Reload files after modification
(global-prettify-symbols-mode   1) ; Pretty symbols (e.g. lambda => λ)
(global-subword-mode            1) ; Better editing of camelCasedWords
(menu-bar-mode                 -1) ; No menu bar
(tool-bar-mode                 -1) ; Don't show tool bar
(prefer-coding-system      'utf-8) ; Always prefer UTF-8
(scroll-bar-mode               -1) ; No scroll bar
(show-paren-mode                1) ; Highlight matching parenthesis
(desktop-save-mode              1) ; remember open files
(save-place-mode                1) ; Remember per-file positions

(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; (load dot-org)
(message "starting dot-org")

;; (add-to-list 'load-path "/var/sharedelpa/stra;; ight/build/org")

;; (load "/var/sharedelpa/straight/build/org/org.el")
;; (require 'org)

(use-package org
  :mode ("\\.org" . org-mode)
  :bind (;; ("M-C"   . jump-to-org-agenda)
         ;; ("C-c o c" . org-capture)
;;; overloaded
         ;; ("M-M"   . org-inline-note)
         ;; ("C-c a" . org-agenda)
         ;; ("C-c C-h" . org-babel-remove-result)
         ("C-c S" . org-store-link)
         ;; ("C-c o l" . org-insert-link)
         )

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
  (org-src-tab-acts-natively t)    ;; indent for src code natively
  (org-src-preserve-indentation nil)
  (org-edit-src-content-indentation t)
  (org-imenu-depth 8)
  (org-src-window-setup 'plain)
  (imenu-auto-rescan t)
  (org-plantuml-jar-path (expand-file-name "/usr/share/java/plantuml.jar"))
  (org-M-RET-may-split-line '((headline) (default . t)))
  (org-adapt-indentation t)
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
  (org-startup-indented t)
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
  :defer t
  :after '(org)
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
  :after '(org hydra)
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
  :after '(org)
  :commands org-bullets-mode
  :hook (org-mode .
                  (lambda ()
                    (org-bullets-mode +1))))

(use-package org-seek
  :after org
  :commands (org-seek-string org-seek-regexp org-seek-headlines))

(use-package org-autolist
  :after '(org))

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
  :after '(org projectile)
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

;; (when nil
;;   (custom-set-faces
;;    '(variable-pitch ((t (:family "ETBembo")))))
;;   ;; (custom-set-faces
;;   ;;  '(org-document-title ((t (:foreground "#171717" :weight bold :height 1.5)))))
;;   (custom-set-faces
;;    '(org-document-title ((t (:foreground "#f7f7f7" :weight bold :height 1.5)))))
;;   ;; (custom-set-faces
;;   ;;  '(org-done ((t (:background "#E8E8E8" :foreground "#0E0E0E" :strike-through t :weight bold)))))
;;   ;; (custom-set-faces
;;   ;;  '(org-headline-done ((t (:foreground "#171717" :strike-through t)))))
;;   ;; (custom-set-faces
;;   ;;  '(org-level-1 ((t (:foreground "#090909" :weight bold :height 1.3)))))
;;   ;; (custom-set-faces
;;   ;;  '(org-level-2 ((t (:foreground "#090909" :weight normal :height 1.2)))))
;;   ;; (custom-set-faces
;;   ;;  '(org-level-3 ((t (:foreground "#090909" :weight normal :height 1.1)))))
;;   (custom-set-faces
;;    '(org-image-actual-width '(600)))
;;   (custom-set-faces
;;    '(org-block-begin-line ((t (:background "#fbf8ef")))))
;;   (custom-set-faces
;;    '(org-block-end-line ((t (:background "#fbf8ef")))))

;;   (setq default-major-mode 'org-mode)

;;   (add-hook 'org-mode-hook
;;             '(lambda ()
;;                (variable-pitch-mode 1) ;; All fonts with variable pitch.
;;                (mapc
;;                 (lambda (face) ;; Other fonts with fixed-pitch.
;;                   (set-face-attribute face nil :inherit 'fixed-pitch))
;;                 (list 'org-code
;;                       'org-link
;;                       'org-block
;;                       'org-table
;;                       'org-verbatim
;;                       'org-block-begin-line
;;                       'org-block-end-line
;;                       'org-meta-line
;;                       'org-document-info-keyword)))))

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

(use-package ob-diagrams
  :after '(org))

(use-package ob-restclient
  :after '(org))

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

(use-package org-bookmark-heading
  :after '(org))

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

(use-package org-generate
  :after '(org))

(use-package org-mime
  :after '(org)
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
  :after '(org)
  :config
  (delete '("\\.pdf\\'" . default) org-file-apps)
  (add-to-list 'org-file-apps '("\\.pdf\\'" . org-pdfview-open))
  (add-to-list 'org-file-apps
               '("\\.pdf::\\([[:digit:]]+\\)\\'" . org-pdfview-open)))

(use-package org-protocol
  :straight nil)

(use-package orca
  :after '(org)
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
  :disabled t
  ;; :straight (:host github :repo "KaratasFurkan/org-rainbow-tags")
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
  :after '(org)
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
  :after '(org)
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

(use-package orgnav
  :after '(org))

(use-package org-jira
  :straight (org-jira :host github :repo "ahungry/org-jira")
  :defer t
  :commands (org-jira-hydra org-jira-select-board org-jira-select-spring)
  :custom (org-jira-property-overrides '("CUSTOM_ID" "self"))
  :bind (:map evil-normal-state-map ("SPC j" . org-jira-hydra))
  :config
  (setq jiralib-url "https://issues.redhat.com/"
        jiralib-user-login-name "ikanello1@redhat.com"
        jira-password nil
        jira-token
        (replace-regexp-in-string
         "\n\\'" ""
         (shell-command-to-string
          "pass show websites/redhat.com/ikanello1@redhat.com/token"))
        org-jira-working-dir (ensure-directory"~/Documents/org/jira/")
        org-jira-projects-list '("ENTSBT" "SB" "QUARKUS"))
  (setq jiralib-token `("Authorization" . ,(concat "Bearer " jira-token)))

  (defvar org-jira-selected-board nil)
  (defvar org-jira-selected-sprint nil)
  (defvar org-jira-selected-epic nil)

  (defvar org-jira-boards-cache ())
  (defvar org-jira-sprint-by-board-cache ())
  (defvar org-jira-epic-by-board-cache ())

  ;;
  ;; Custom functions
  ;;

  ;;
  ;; Boards
  ;;
  (defun org-jira-get-boards-list()
    "List all boards."
    (unless org-jira-boards-cache
      (setq org-jira-boards-cache (jiralib--agile-call-sync "/rest/agile/1.0/board" 'values)))
    org-jira-boards-cache)

  (defun org-jira-get-board-id()
    "Select a board if one not already selected."
    (unless org-jira-selected-board
      (setq org-jira-selected-board (org-jira-board-completing-read)))
    (cdr (assoc 'id org-jira-selected-board)))

  (defun org-jira-get-board()
    "Select a board if one not already selected."
    (unless org-jira-selected-board
      (setq org-jira-selected-board (org-jira-board-completing-read)))
    org-jira-selected-board)

  (defun org-jira-board-completing-read()
    "Select a board by name."
    (when (not (file-exists-p (org-jira--get-boards-file)))
      (org-jira-get-boards-list))

    (let* ((boards (with-current-buffer (org-jira--get-boards-buffer)
                     (org-map-entries (lambda()
                                        `((id . ,(org-entry-get nil "id"))
                                          (self . ,(org-entry-get nil "url"))
                                          (name . ,(org-entry-get nil "name")))) t  'file)))
           (board-names (mapcar #'(lambda (a) (cdr (assoc 'name a))) boards))
           (board-name (completing-read "Choose board:" board-names)))
      (car (seq-filter #'(lambda (a) (equal (cdr (assoc 'name a)) board-name)) boards))))

  (defun org-jira-select-board()
    "Select a board."
    (interactive)
    (setq org-jira-selected-board (cdr (assoc 'name (org-jira-board-completing-read)))))

  ;;
  ;; Sprint
  ;;
  (defun org-jira-get-project-boards(project-id)
    "Find the board of the project.")

  (defun org-jira-get-sprints-by-board(board-id &optional filter)
    "List all sprints by BOARD-ID."
    (let ((board-sprints-cache (cdr (assoc board-id org-jira-sprint-by-board-cache))))
      (unless board-sprints-cache
        (setq board-sprints-cache (jiralib--agile-call-sync (format "/rest/agile/1.0/board/%s/sprint" board-id)'values)))

      (add-to-list 'org-jira-sprint-by-board-cache `(,board-id . ,board-sprints-cache))
      (if filter
          (seq-filter filter board-sprints-cache)
        board-sprints-cache)))

  (defun org-jira--active-sprint-p(sprint)
    "Predicate that checks if SPRINT is active."
    (not (assoc 'completeDate sprint)))

  (defun org-jira-sprint-completing-read(board-id)
    "Select an active sprint by name."
    (let* ((sprints (org-jira-get-sprints-by-board board-id 'org-jira--active-sprint-p))
           (sprint-names (mapcar #'(lambda (a) (cdr (assoc 'name a))) sprints))
           (sprint-name (completing-read "Choose sprint:" sprint-names)))
      (car (seq-filter #'(lambda (a) (equal (cdr (assoc 'name a)) sprint-name)) sprints))))

  (defun org-jira-move-issue-to-sprint(issue-id sprint-id)
    "Move issue with ISSUE-ID to sprint with SPRINT-ID."
    (jiralib--rest-call-it (format "/rest/agile/1.0/sprint/%s/issue" sprint-id) :type "POST" :data (format "{\"issues\": [\"%s\"]}" issue-id)))

  (defun org-jira-assign-current-issue-to-sprint()
    "Move the selected issue to an active sprint."
    (interactive)
    (let* ((issue-id (org-jira-parse-issue-id))
           (board-id (cdr (assoc 'id (org-jira-get-board))))
           (sprint-id (cdr (assoc 'id (org-jira-sprint-completing-read board-id)))))

      (org-jira-move-issue-to-sprint issue-id sprint-id)))

  (defun org-jira-get-sprint-id()
    "Select a sprint id if one not already selected."
    (unless org-jira-selected-sprint
      (setq org-jira-selected-sprint (org-jira-sprint-completing-read)))
    (cdr (assoc 'id org-jira-selected-sprint)))

  (defun org-jira-get-sprint()
    "Select a sprint if one not already selected."
    (unless org-jira-selected-sprint
      (setq org-jira-selected-sprint (org-jira-select-sprint)))
    org-jira-selected-sprint)

  (defun org-jira-select-sprint()
    "Select a sprint."
    (interactive)
    (setq org-jira-selected-sprint (org-jira-sprint-completing-read (org-jira-get-board-id))))

  ;;
  ;; Epics
  ;;
  (defun org-jira-get-epics-by-board(board-id &optional filter)
    "List all epics by BOARD-ID."
    (interactive)
    (let ((board-epics-cache (cdr (assoc board-id org-jira-epic-by-board-cache))))
      (unless board-epics-cache
        (setq board-epics-cache (jiralib--agile-call-sync (format "/rest/agile/1.0/board/%s/epic" board-id)'values)))

      (add-to-list 'org-jira-epic-by-board-cache `(,board-id . ,board-epics-cache))
      (if filter
          (seq-filter filter board-epics-cache)
        board-epics-cache)))

  (defun org-jira--active-epic-p(epic)
    "Predicate that checks if EPIC is active."
    (not (equal (assoc 'done epic) 'false)))


  (defun org-jira-epic-completing-read(board-id)
    "Select an active epic by name."
    (let* ((epics (org-jira-get-epics-by-board board-id 'org-jira--active-epic-p))
           (epic-names (mapcar #'(lambda (a) (cdr (assoc 'name a))) epics))
           (epic-name (completing-read "Choose epic:" epic-names)))
      (car (seq-filter #'(lambda (a) (equal (cdr (assoc 'name a)) epic-name)) epics))))

  (defun org-jira-move-issue-to-epic(issue-id epic-id)
    "Move issue with ISSUE-ID to epic with SPRINT-ID."
    (jiralib--rest-call-it (format "/rest/agile/1.0/epic/%s/issue" epic-id) :type "POST" :data (format "{\"issues\": [\"%s\"]}" issue-id)))

  (defun org-jira-assign-current-issue-to-epic()
    "Move the selected issue to an active epic."
    (interactive)
    (let* ((issue-id (org-jira-parse-issue-id))
           (board-id (cdr (assoc 'id (org-jira-get-board))))
           (epic-id (cdr (assoc 'id (org-jira-epic-completing-read board-id)))))

      (org-jira-move-issue-to-epic issue-id epic-id)))

  (defun org-jira-get-epic-id()
    "Select a epic id if one not already selected."
    (unless org-jira-selected-epic
      (setq org-jira-selected-epic (org-jira-epic-completing-read)))
    (cdr (assoc 'id org-jira-selected-epic)))

  (defun org-jira-get-epic()
    "Select a epic if one not already selected."
    (unless org-jira-selected-epic
      (setq org-jira-selected-epic (org-jira-select-epic)))
    org-jira-selected-epic)

  (defun org-jira-select-epic()
    "Select a epic."
    (interactive)
    (setq org-jira-selected-epic (org-jira-epic-completing-read (org-jira-get-board-id))))

  (defun org-jira-create-issue-with-defaults()
    "Create an issue and assign to default sprint and epic."
    (org-jira-create-issue)
    (org-jira-move-issue-to-epic)
    (org-jira-move-issue-to-sprint))

  (defun org-jira-update-issue-description()
    "Move the selected issue to an active sprint."
    (interactive)
    (let* ((issue-id (org-jira-parse-issue-id))
           (filename (buffer-file-name))
           (org-issue-description (org-trim (org-jira-get-issue-val-from-org 'description)))
           (update-fields (list (cons 'description org-issue-description))))
      (message "Updating issue:%s from file: %s with description:%s" issue-id filename org-issue-description)
      (jiralib-update-issue issue-id update-fields
                            (org-jira-with-callback
                              (message (format "Issue '%s' updated!" issue-id))
                              (jiralib-get-issue
                               issue-id
                               (org-jira-with-callback
                                 (org-jira-log "Update get issue for refresh callback hit.")
                                 (-> cb-data list org-jira-get-issues))))
                            )))


;;;###autoload
  (defun ic/org-jira-postprocess ()
    "Postprocess the org-jira project files."
    (interactive)
    (require 'org-sync-github)
    (mapcar (lambda (p)
              (let ((scheduled (format "%s  SCHEDULED: <%s>\n" (make-string 2 32) (org-read-date nil nil "+0d") ))
                    (github-tasks-file (format "~/Documents/org/jira/%s.org" p)))
                (with-temp-buffer
                  (insert-file github-tasks-file)
                  (goto-char (point-min))
                  (while (re-search-forward "^\*\* TODO" nil t)
                    (message "Setting scheduled and tags")
                    (let* ((tags (org-get-tags)))
                      (add-to-list 'tags "jira")
                      (org-set-tags tags)
                      (org-set-property "SCHEDULED" scheduled)
                      (write-file github-tasks-file)))))) '("QUARKUS" "SB" "ENTSBT")))

  (defun ic/org-jira-get-issues ()
    "Sync using org-jira and postprocess."
    (interactive)
    (org-jira-get-issues (org-jira-get-issue-list org-jira-get-issue-list-callback))
    (ic/org-jira-postprocess))

  (defun org-jira-issue-id-at-point ()
    "Returns the ID of the current issue."
    (save-excursion
      (org-previous-visible-heading 1)
      (org-element-property :ID (org-element-at-point))))

  (defun org-jira-hydra ()
    "Define (if not already defined org-jira hydra and invoke it."
    (interactive)
    (unless (boundp 'org-jira-hydra/body)
      (defhydra org-jira-hydra (:hint none :exit t)
        ;; The '_' character is not displayed. This affects columns alignment.
        ;; Remove s many spaces as needed to make up for the '_' deficit.
        "
         ^Actions^           ^Issue^              ^Buffer^                         ^Defaults^ 
                           ?I?
         ^^^^^^-----------------------------------------------------------------------------------------------
          _L_ist issues      _u_pdate issue       _R_efresh issues in buffer       Select _B_oard ?B?
          _C_reate issue     update _c_omment                                    Select _E_pic ?E?
                           assign _s_print                                     Select _S_print ?S?
                           assign _e_print                                     Create issue with _D_efaults
                           _b_rowse issue
                           _r_efresh issue
                           _p_rogress issue
  [_q_]: quit
"
        ("I" nil (or (org-jira-issue-id-at-point) ""))
        ("L" ic/org-jira-get-issues)
        ("C" org-jira-create-issue)

        ("u" org-jira-update-issue)
        ("c" org-jira-update-comment)
        ("b" org-jira-browse-issue)
        ("s" org-jira-assign-current-issue-to-sprint)
        ("e" org-jira-assign-current-issue-to-epic)
        ("r" org-jira-refresh-issue)
        ("p" org-jira-progress-issue)

        ("R" org-jira-refresh-issues-in-buffer)

        ("B" org-jira-select-board (format "[%s]" (or org-jira-selected-board "")) :exit nil)
        ("E" org-jira-select-epic (format "[%s]" (or org-jira-selected-epic "")) :exit nil)
        ("S" org-jira-select-sprint (format "[%s]" (or org-jira-selected-sprint "")) :exit nil)
        ("D" org-jira-create-with-defaults)

        ("q" nil "quit")))
    (org-jira-hydra/body))
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
  :commands org-tree-slide-mode
  :custom
  (org-image-actual-width nil)
  :bind (:map org-tree-slide-mode-map
              (("C-c n" . org-tree-slide-move-next-tree)
               ("C-c p" . org-tree-slide-move-previous-tree))))

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

(use-package orgtbl-aggregate
  :after '(org))

(use-package ox-gfm
  :after '(org)
  ;;
  ;; :commands ox-gfm-export-to-markdown
  )

(use-package ox-jira
  :commands ox-jira-export-as-jira)

(use-package git-link
  :bind (("C-c Y" . git-link)))

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

(if window-system
    (add-hook 'after-init-hook #'toggle-frame-maximized))

;; Automatically create any nonexistent parent directories when
;; finding a file. If the buffer for the new file is killed without
;; being saved, then offer to delete the created directory or
;; directories.

(defun my-find-file-automatically-create-directory
    (find-file filename &optional wildcards)
  "Advice for FIND-FILE' to create missing path elements.
Missing directories from topmost root to leaf in FILENAME will be
created as necessary. WILDCARDS are passed through when present
It also sets a buffer-local variable so that the user will be
prompted to delete the newly created directories if they kill the
buffer without saving it."
  ;; The variable `dirs-to-delete' is a list of the directories that
  ;; will be automatically created by `make-directory'. We will want
  ;; to offer to delete these directories if the user kills the buffer
  ;; without saving it.
  (let ((dirs-to-delete ()))
    ;; If the file already exists, we don't need to worry about
    ;; creating any directories.
    (unless (file-exists-p filename)
      ;; It's easy to figure out how to invoke `make-directory',
      ;; because it will automatically create all parent directories.
      ;; We just need to ask for the directory immediately containing
      ;; the file to be created.
      (let* ((dir-to-create (file-name-directory filename))
             ;; However, to find the exact set of directories that
             ;; might need to be deleted afterward, we need to iterate
             ;; upward through the directory tree until we find a
             ;; directory that already exists, starting at the
             ;; directory containing the new file.
             (current-dir dir-to-create))
        ;; If the directory containing the new file already exists,
        ;; nothing needs to be created, and therefore nothing needs to
        ;; be destroyed, either.
        (while (not (file-exists-p current-dir))
          ;; Otherwise, we'll add that directory onto the list of
          ;; directories that are going to be created.
          (push current-dir dirs-to-delete)
          ;; Now we iterate upwards one directory. The
          ;; `directory-file-name' function removes the trailing slash
          ;; of the current directory, so that it is viewed as a file,
          ;; and then the `file-name-directory' function returns the
          ;; directory component in that path (which means the parent
          ;; directory).
          (setq current-dir (file-name-directory
                             (directory-file-name current-dir))))
        ;; Only bother trying to create a directory if one does not
        ;; already exist.
        (unless (file-exists-p dir-to-create)
          ;; Make the necessary directory and its parents.
          (make-directory dir-to-create 'parents))))
    ;; Call the original `find-file', now that the directory
    ;; containing the file to found exists.
    (funcall find-file filename wildcards)
    ;; If there are directories we want to offer to delete later, we
    ;; have more to do.
    (when dirs-to-delete
      ;; Since we already called `find-file', we're now in the buffer
      ;; for the new file. That means we can transfer the list of
      ;; directories to possibly delete later into a buffer-local
      ;; variable. But we pushed new entries onto the beginning of
      ;; `dirs-to-delete', so now we have to reverse it (in order to
      ;; later offer to delete directories from innermost to
      ;; outermost).
      (setq-local my-dirs-to-delete (reverse dirs-to-delete))
      ;; Now we add a buffer-local hook to offer to delete those
      ;; directories when the buffer is killed, but only if it's
      ;; appropriate to do so (for instance, only if the directories
      ;; still exist and the file still doesn't exist).
      (add-hook 'kill-buffer-hook
                #'my-kill-buffer-delete-directory-if-appropriate
                'append 'local)
      ;; The above hook removes itself when it is run, but that will
      ;; only happen when the buffer is killed (which might never
      ;; happen). Just for cleanliness, we automatically remove it
      ;; when the buffer is saved. This hook also removes itself when
      ;; run, in addition to removing the above hook.
      (add-hook 'after-save-hook
                #'my-remove-kill-buffer-delete-directory-hook
                'append 'local))))

;; Add the advice that we just defined.
(advice-add #'find-file :around
            #'my-find-file-automatically-create-directory)

(defun my-kill-buffer-delete-directory-if-appropriate ()
  "Offer to delete directories created by This hook.
the directory containing the file for the current buffer
automatically, then offer to delete it. Otherwise, do nothing.
Also clean up related hooks."
  (when (and
         ;; Stop if there aren't any directories to delete (shouldn't
         ;; happen).
         my-dirs-to-delete
         ;; Stop if `my-dirs-to-delete' somehow got set to
         ;; something other than a list (shouldn't happen).
         (listp my-dirs-to-delete)
         ;; Stop if the current buffer doesn't represent a
         ;; file (shouldn't happen).
         buffer-file-name
         ;; Stop if the buffer has been saved, so that the file
         ;; actually exists now. This might happen if the buffer were
         ;; saved without `after-save-hook' running.
         (not (file-exists-p buffer-file-name)))
    (cl-dolist (dir-to-delete my-dirs-to-delete)
      ;; Ignore any directories that no longer exist or are malformed.
      ;; We don't return immediately if there's a nonexistent
      ;; directory, because it might still be useful to offer to
      ;; delete other (parent) directories that should be deleted. But
      ;; this is an edge case.
      (when (and (stringp dir-to-delete)
                 (file-exists-p dir-to-delete))
        ;; Only delete a directory if the user is OK with it.
        (if (y-or-n-p (format "Also delete directory `%s'? "
                              ;; The `directory-file-name' function
                              ;; removes the trailing slash.
                              (directory-file-name dir-to-delete)))
            (delete-directory dir-to-delete)
          ;; If the user doesn't want to delete a directory, then they
          ;; obviously don't want to delete any of its parent
          ;; directories, either.
          (cl-return)))))
  ;; It shouldn't be necessary to remove this hook, since the buffer
  ;; is getting killed anyway, but just in case...
  (my-remove-kill-buffer-delete-directory-hook))

(defun my-remove-kill-buffer-delete-directory-hook ()
  "Unhook `my-kill-buffer-delete-directory-if-appropriate'.
Remove from `kill-buffer-hook', and also remove this function
 from `after-save-hook'."
  (remove-hook 'kill-buffer-hook
               #'my-kill-buffer-delete-directory-if-appropriate
               'local)
  (remove-hook 'after-save-hook
               #'my-remove-kill-buffer-delete-directory-hook
               'local))

;; set major mode of unsaved buffer to file extension
(setq-default major-mode
              (lambda () (if buffer-file-name
			(fundamental-mode)
		      (let ((buffer-file-name (buffer-name)))
			(set-auto-mode)))))

;;;_ , Utility macros and functions

(defun largest-window-matching (pred)
  "Return largest window in current frame matching PRED."
  (elt (car (sort (mapcar
                   (lambda (w)
                     (if (funcall pred w)
                         (vector (* (window-total-height w)
                                    (window-total-width w)) w)
                       (vector 0 nil))) (window-list))
                  (lambda (x y) (> (elt x 0) (elt y 0))))) 1))

(defun system-idle-time ()
  "Rreturn floating idle time."
  (with-temp-buffer
    (call-process "ioreg" nil (current-buffer) nil
                  "-c" "IOHIDSystem" "-d" "4" "-S")
    (goto-char (point-min))
    (and (re-search-forward "\"HIDIdleTime\" = \\([0-9]+\\)" nil t)
         (/ (float (string-to-number (match-string 1)))
            1000000000.0))))

(defun quickping (host)
  "Return ping time for HOST."
  (= 0 (call-process "/bin/ping" nil nil nil "-c1" "-W50" "-q" host)))

(defsubst hook-into-modes (func &rest modes)
  "Hook multiple modes at once.  FUNC and MODES are self-explanatory."
  (dolist (mode-hook modes) (add-hook mode-hook func)))

(eval-and-compile
  (define-key ctl-x-map "\C-i"
    #'endless/ispell-word-then-abbrev)

  (defun endless/simple-get-word ()
    (car-safe (save-excursion (ispell-get-word nil))))

  (defun endless/ispell-word-then-abbrev (p)
    "Call `ispell-word', then create an abbrev for it.
With prefix P, create local abbrev. Otherwise it will
be global.
If there's nothing wrong with the word at point, keep
looking for a typo until the beginning of buffer. You can
skip typos you don't want to fix with `SPC', and you can
abort completely with `C-g'."
    (interactive "P")
    (let (bef aft)
      (save-excursion
        (while (if (setq bef (endless/simple-get-word))
                   ;; Word was corrected or used quit.
                   (if (ispell-word nil 'quiet)
                       nil ; End the loop.
                     ;; Also end if we reach `bob'.
                     (not (bobp)))
                 ;; If there's no word at point, keep looking
                 ;; until `bob'.
                 (not (bobp)))
          (backward-word)
          (backward-char))
        (setq aft (endless/simple-get-word)))
      (if (and aft bef (not (equal aft bef)))
          (let ((aft (downcase aft))
                (bef (downcase bef)))
            (define-abbrev
              (if p local-abbrev-table global-abbrev-table)
              bef aft)
            (message "\"%s\" now expands to \"%s\" %sally"
                     bef aft (if p "loc" "glob")))
        (user-error "No typo at or before point")))))

(use-package alert         :defer t)
(use-package anaphora      :defer t)
(use-package apiwrap       :defer t)
(use-package asoc
  :defer t
  :straight (asoc :host github :repo "troyp/asoc.el"))
(use-package async         :defer t)
(use-package button-lock   :defer t)
(use-package ctable        :defer t)
(use-package dash          :defer t)
(use-package deferred      :defer t)
(use-package diminish      )
(use-package el-mock       :defer t)
(use-package elisp-refs    :defer t)
(use-package epc           :defer t)
(use-package epl           :defer t)
(use-package esxml         :defer t)
(use-package f             :defer t)
(use-package fn            :defer t)
(use-package fringe-helper :defer t)
(use-package fuzzy         :defer t)

(use-package ghub
  :defer t
  :config
  (require 'auth-source-pass)
  (defvar my-ghub-token-cache nil)
  (advice-add
   'ghub--token :around
   #'(lambda (orig-func host username package &optional nocreate forge)
       (or my-ghub-token-cache
           (setq my-ghub-token-cache
                 (funcall orig-func host username package nocreate forge))))))

(use-package ghub+         :defer t)
(use-package ht            :defer t)
(use-package kv            :defer t)
(use-package list-utils    :defer t)
(use-package logito        :defer t)
(use-package loop          :defer t)
(use-package m-buffer      :defer t)
(use-package makey         :defer t)
(use-package marshal       :defer t)
(use-package names         :defer t)
(use-package noflet        :defer t)
(use-package oauth2        :defer t)
(use-package ov            :defer t)
(use-package packed        :defer t)
(use-package parent-mode   :defer t)
(use-package parsebib      :defer t)
(use-package parsec        :defer t)
(use-package peval         :defer t
  :straight (peval :host github :repo "Wilfred/peval"))
(use-package pfuture       :defer t)
(use-package pkg-info      :defer t)
(use-package popup         :defer t)
(use-package popup-pos-tip :defer t)
(use-package popwin        :defer t)
(use-package pos-tip       :defer t)
(use-package request       :defer t)
(use-package rich-minority :defer t)
(use-package s             :defer t)
(use-package simple-httpd  :defer t)
(use-package spinner       :defer t)
(use-package tablist       :defer t)
(use-package uuidgen       :defer t)
(use-package web           :defer t)
(use-package web-server    :defer t :straight nil)
(use-package websocket     :defer t)
(use-package with-editor   :defer t)
(use-package xml-rpc       :defer t)
(use-package zoutline      :defer t)

(use-package crm-prompt
  :unless noninteractive
  :straight nil
  :no-require t
  :preface
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator))

(use-package dubcaps-mode
  :unless noninteractive
  :straight nil
  :preface
  (defun dcaps-to-scaps ()
    "Convert word in DOuble CApitals to Single Capitals."
    (interactive)
    (and (= ?w (char-syntax (char-before)))
         (save-excursion
           (let ((end (point)))
             (and (if (called-interactively-p)
                      (skip-syntax-backward "w")
                    (= -3 (skip-syntax-backward "w")))
                  (let (case-fold-search)
                    (looking-at "\\b[[:upper:]]\\{2\\}[[:lower:]]"))
                  (capitalize-region (point) end))))))

  (define-minor-mode dubcaps-mode
    "Toggle `dubcaps-mode'.  Converts words in DOuble CApitals to
Single Capitals as you type."
    :init-value nil
    :lighter (" DC")
    (if dubcaps-mode
        (add-hook 'post-self-insert-hook #'dcaps-to-scaps nil 'local)
      (remove-hook 'post-self-insert-hook #'dcaps-to-scaps 'local)))
  :commands (dubcaps-mode)
  :hook ((erc-mode
          prog-mode
          text-mode
          tabulated-list-mode) . #'dubcaps-mode))

(use-package set-scroll-margin
  :unless noninteractive
  :straight nil
  :preface
  (defun set-scroll-margin ()
    "Make scroll margins a quarter of the window height."
    (interactive)
    (setq-local scroll-margin (/ (window-height) 4)))
  :commands (set-scroll-margin)
  :hook (after-init . (lambda () (hook-into-modes #'set-scroll-margin
                                             prog-mode
                                             text-mode
                                             dired-mode
                                             tabulated-list-mode)))
  :hook ((prog-mode
          text-mode
          dired-mode
          tabulated-list-mode) . #'set-scroll-margin))

(use-package start-per-user-server
  :unless noninteractive
  :straight nil
  :defer 5
  :preface
  (require 'server)

  (defun server-running-here-p ()
    "Is an emacs server FOR THE CURRENT USER running in this process?"
    (and (fboundp 'server-running-p)
         (server-running-p (getenv "USER"))))

  (defun server-running-elsewhere-p ()
    "Is an emacs server FOR THE CURRENT USER running in any process?"
    (and (not (server-running-here-p))
         (file-exists-p
          (concat (file-name-as-directory "/tmp")
                  (getenv "USER")
                  (number-to-string (user-uid))
                  "/emacs"))))

  (defun start-per-user-server ()
    "Start emacs server for ’$USER’ unless one is already running."
    (interactive)
    (let* ((username (getenv "USER")))
      (message "per-user server for user %s is %s" username (if (server-running-here-p) "running" "not running"))
      (cond
       ((eq t (server-running-here-p))
        (message "This %s server is already running" username) t)
       ((eq t (server-running-elsewhere-p))
        (message "Another %s server is already running" username))
       (t (progn
            (setq server-name username)
            (server-start)
            (message "Started server %s" username))))))

  (defun keep-trying-to-start-server ()
    "Try to start server until it works.

A running server is responsible for servicing all emacsclient
requests, which includes ’lookup-password’ queries from other
tools."
    (interactive)
    (while (not (server-running-here-p))
      (message "got here")
      (run-at-time "5 min" nil #'start-per-user-server)))
  :commands (start-per-user-server keep-trying-to-start-server)
  :config (keep-trying-to-start-server))

(use-package scroll-on-jump
  :config
  (setq scroll-on-jump-duration 0.6))

;; (eval-and-compile
;;   (defun server-running-here-p ()
;;     "Is an emacs server FOR THE CURRENT USER running in this process?"
;;     (and (fboundp 'server-running-p)
;;          (server-running-p (getenv "USER"))))

;;   (defun server-running-elsewhere-p ()
;;     "Is an emacs server FOR THE CURRENT USER running in any process?"
;;     (and (not (server-running-here-p))
;;          (file-exists-p
;;           (concat (file-name-as-directory "/tmp")
;;                   (getenv "USER")
;;                   (number-to-string (user-uid))
;;                   "/emacs"))))

;;   (defun start-per-user-server ()
;;     "Start emacs server for ’$USER’ unless one is already running."
;;     (interactive)
;;     (let* ((username (getenv "USER")))
;;       (cond
;;        ((eq t (server-running-here-p))
;;         (message "This %s server is already running" username) t)
;;        ((eq t (server-running-elsewhere-p))
;;         (message "Another %s server is already running" username))
;;        (t (progn)))
;;       (setq server-name username)
;;       (server-start)
;;       (message "Started server %s" username)))

;;   (defun keep-trying-to-start-server ()
;;     "Try to start server until it works.

;; A running server is responsible for servicing all emacsclient
;; requests, which includes ’lookup-password’ queries from other
;; tools."
;;     (interactive)
;;     (while (not (server-running-here-p))
;;       (run-at-time "5 min" nil #'start-per-user-server)))
;;   (add-hook 'after-init-hook #'keep-trying-to-start-server))

;;;_ , Spacemacs runoff

;; deleted "" prefixes from function names for ivy findability
(eval-and-compile
  (defvar useless-buffers-regexp '("*\.\+")
    "Regexp used to determine if a buffer is not useful.")
  (defvar useful-buffers-regexp
    '("\\*\\(scratch\\|terminal\.\+\\|ansi-term\\|eshell\\)\\*")
    "Regexp for useful buffers despite matching `useless-buffers-regexp'.")

  (defun useless-buffer-p (buffer)
    "Determines if a BUFFER is useful."
    (let ((buf-paren-major-mode (get (with-current-buffer buffer
                                       major-mode)
                                     'derived-mode-parent))
          (buf-name (buffer-name buffer)))
      ;; first find if useful buffer exists, if so returns nil and don't check for
      ;; useless buffers. If no useful buffer is found, check for useless buffers.
      (unless (cl-loop for regexp in useful-buffers-regexp do
                       (when (or (eq buf-paren-major-mode 'comint-mode)
                                 (string-match regexp buf-name))
                         (return t)))
        (cl-loop for regexp in useless-buffers-regexp do
                 (when (string-match regexp buf-name)
                   (return t))))))

  (defun next-useful-buffer ()
    "Switch to the next buffer and avoid special buffers."
    (interactive)
    (let ((start-buffer (current-buffer)))
      (next-buffer)
      (while (and (useless-buffer-p (current-buffer))
                  (not (eq (current-buffer) start-buffer)))
        (next-buffer))))

  (defun previous-useful-buffer ()
    "Switch to the previous buffer and avoid special buffers."
    (interactive)
    (let ((start-buffer (current-buffer)))
      (previous-buffer)
      (while (and (useless-buffer-p (current-buffer))
                  (not (eq (current-buffer) start-buffer)))
        (previous-buffer))))

  (defun kill-file-name ()
    "Put the current buffer’s file name on the `kill-ring`."
    (interactive)
    (let ((filename (if (equal major-mode 'dired-mode)
                        default-directory
                      (buffer-file-name))))
      (when filename
        (with-temp-buffer
          (insert filename)
          (clipboard-kill-region (point-min) (point-max)))
        (message filename)))))

;; from magnars
(eval-and-compile
  (defun rename-current-buffer-file ()
    "Renames current buffer and file it is visiting."
    (interactive)
    (let ((name (buffer-name))
          (filename (buffer-file-name)))
      (if (not (and filename (file-exists-p filename)))
          (error "Buffer '%s' is not visiting a file!" name)
        (let ((new-name (read-file-name "New name: " filename)))
          (cond ((get-buffer new-name)
                 (error "A buffer named '%s' already exists!" new-name))
                (t
                 (let ((dir (file-name-directory new-name)))
                   (when (and (not (file-exists-p dir))
                              (yes-or-no-p (format "Create directory '%s'?" dir)))
                     (make-directory dir t)))
                 (rename-file filename new-name 1)
                 (rename-buffer new-name)
                 (set-visited-file-name new-name)
                 (set-buffer-modified-p nil)
                 (when (fboundp 'recentf-add-file)
                   (recentf-add-file new-name)
                   (recentf-remove-if-non-kept filename))
                 (message "File '%s' successfully renamed to '%s'"
                          name (file-name-nondirectory new-name))))))))

  ;; from magnars

  (defun delete-current-buffer-file ()
    "Remove file connected to current buffer and kill buffer."
    (interactive)
    (let ((filename (buffer-file-name))
          (buffer (current-buffer))
          (name (buffer-name)))
      (if (not (and filename (file-exists-p filename)))
          (ido-kill-buffer)
        (when (yes-or-no-p "Are you sure you want to delete this file? ")
          (delete-file filename t)
          (kill-buffer buffer)
          (message "File '%s' successfully removed" filename))))))

;; found at http://emacswiki.org/emacs/KillingBuffers
(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (when (yes-or-no-p
         (format "Killing all buffers except \"%s\"? " (buffer-name)))
    (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
    (message "Buffers deleted!")))

;; http://camdez.com/blog/2013/11/14/emacs-show-buffer-file-name/
(defun show-and-copy-buffer-filename ()
  "Show the full path to the current file in the minibuffer."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if file-name
        (progn
          (message file-name)
          (kill-new file-name))
      (error "Buffer not visiting a file"))))

;; apparently the only way to determine which desktop shell a linux system is
;; operating in is to look for telltale processes, so here's a utility to help.

;; (defun process-names-matching (r)
;;   "Return a list of process names matching R or nil."
;;   (seq-filter
;;    (lambda (x)
;;      (and x (string-match r x)))
;;    (map 'list
;;         #'(lambda (p)
;;             (alist-get 'comm (process-attributes p)))
;;         (list-system-processes))))

;; (defun linux-gnome-p ()
;;   "Is the system running gnome?"
;;   (not (null (process-names-matching "gnome"))))

;; (defun shell-command-to-string (&rest cmd)
;;   "Fork shell,CMD and clean resulting string."
;;   (replace-regexp-in-string
;;    "\r?\n$" ""
;;    (shell-command-to-string
;;     (mapconcat 'identity cmd "
;; "))))

;; No test data for multiple profiles. Probable error if that ever occurs.
(defun gnome-terminal-profile ()
  "Return default gnome profile."
  (shell-command-to-string "dconf list /org/gnome/terminal/legacy/profiles:/"))

;;;_ , Enable disabled commands

(setq disabled-command-function nil) ;; enable all commands

;; (put 'downcase-region  'disabled nil)   ; Let downcasing work
;; (put 'erase-buffer     'disabled nil)
;; (put 'eval-expression  'disabled nil)   ; Let ESC-ESC work
;; (put 'narrow-to-page   'disabled nil)   ; Let narrowing work
;; (put 'narrow-to-region 'disabled nil)   ; Let narrowing work
;; (put 'set-goal-column  'disabled nil)
;; (put 'upcase-region    'disabled nil)   ; Let upcasing work

;; (put 'company-coq-fold            'disabled nil)
;; (put 'TeX-narrow-to-group         'disabled nil)
;; (put 'LaTeX-narrow-to-environment 'disabled nil)

(eval-and-compile
  (mapc #'(lambda (entry)
            (define-prefix-command (cdr entry))
            (bind-key (car entry) (cdr entry)))
        '(
          ;; ("C-,"   . my-ctrl-comma-map)
          ("<C-m>" . my-ctrl-m-map)

          ;; ("C-h e" . my-ctrl-h-e-map)
          ;; ("C-h x" . my-ctrl-h-x-map)

          ;; ("C-c b" . my-ctrl-c-b-map)
          ;; ("C-c e" . my-ctrl-c-e-map)
          ;; ("C-c m" . my-ctrl-c-m-map)
          ;; ("C-c w" . my-ctrl-c-w-map)
          ;; ("C-c y" . my-ctrl-c-y-map)
          ;; ("C-c H" . my-ctrl-c-H-map)
          ;; ("C-c N" . my-ctrl-c-N-map)
          ;; ("C-c (" . my-ctrl-c-open-paren-map)
          ;; ("C-c -" . my-ctrl-c-minus-map)
          ;; ("C-c =" . my-ctrl-c-equals-map)
          ;; ("C-c ." . my-ctrl-c-r-map)
          )))

;;;_. Keybindings

;;;_ , global-map

;;;_ , tab

;; (defun smart-tab (&optional arg)
;;   "Make the tab key expand and complete with ARG."
;;   (interactive "P")
;;   (message "smart-tab somehow")
;;   (cond
;;    ((looking-back "^[-+* \t]*" nil)
;;     (if (eq major-mode 'org-mode)
;;       (org-cycle arg)
;;       (indent-according-to-mode)))
;;    (t
;;     ;; Hippie also expands yasnippets, due to `yas-hippie-try-expand' in
;;     ;; `hippie-expand-try-functions-list'.
;;     (hippie-expand arg))))

;; (bind-key "<tab>" #'smart-tab)

(define-key key-translation-map (kbd "A-TAB") (kbd "C-TAB"))

;; (defun check-expansion ()
;;   (save-excursion
;;     (if (looking-at "\\_>") t
;;       (backward-char 1)
;;       (if (looking-at "\\.") t
;;         (backward-char 1)
;;         (if (looking-at "->") t nil)))))

;; (defun do-yas-expand ()
;;   (let ((yas/fallback-behavior 'return-nil))
;;     (yas/expand)))

;; (defun tab-indent-or-complete ()
;;   (interactive)
;;   (if (minibufferp)
;;       (minibuffer-complete)
;;     (if (or (not yas-minor-mode)
;;             (null (do-yas-expand)))
;;         (if (check-expansion)
;;             ;;(auto-complete)
;;             (company-complete-common)
;;           (indent-for-tab-command)))))

;;;_  . C-

(defun cycle-bol-boi ()
  "Cycle between beginning of line and beginning of indentation."
  (interactive)
  (let ((orig (point)))
    (back-to-indentation)
    (when (= orig (point))
      (move-beginning-of-line 1))))

(bind-key "C-a" 'cycle-bol-boi)

;; (defvar ctl-period-map)
;; (define-prefix-command 'ctl-period-map)
;; (bind-key "C-." 'ctl-period-map)

(bind-key* "C-\\" 'switch-to-buffer)
(bind-key* "<C-return>" 'other-window)

(defun collapse-or-expand ()
  "Delete other windows."
  (interactive)
  (if (> (length (window-list)) 1)
      (delete-other-windows)
    (bury-buffer)))

(bind-key "C-z" 'collapse-or-expand)

;;;_  . M-

(defadvice async-shell-command (before uniqify-running-shell-command activate)
  "Execute command without blocking."
  (let ((buf (get-buffer "*Async Shell Command*")))
    (if buf
        (let ((proc (get-buffer-process buf)))
          (if (and proc (eq 'run (process-status proc)))
              (with-current-buffer buf
                (rename-uniquely)))))))

(bind-key "M-!" 'async-shell-command)
(bind-key "M-'" 'insert-pair)
;;(bind-key "M-\"" 'insert-pair)

;;;; obsoleted by aggressive-indent
;; (defun align-code (beg end &optional arg)
;;   (interactive "rP")
;;   (if (null arg)
;;       (align beg end)
;;     (let ((end-mark (copy-marker end)))
;;       (indent-region beg end-mark nil)
;;       (align beg end-mark))))

;; (bind-key "M-[" 'align-code)

(bind-key "M-`" 'other-frame)

(bind-key "M-j" 'delete-indentation-forward)
(bind-key "M-J" 'delete-indentation)
(bind-key "M-W" 'mark-word)

(defun mark-line (&optional arg)
  "Mark ARG lines."
  (interactive "p")
  (beginning-of-line)
  (let ((here (point)))
    (dotimes (i arg)
      (end-of-line))
    (set-mark (point))
    (goto-char here)))

(bind-key "M-L" 'mark-line)

(defun mark-sentence (&optional arg)
  "Mark ARG sentences."
  (interactive "P")
  (backward-sentence)
  (mark-end-of-sentence arg))

(bind-key "M-S" 'mark-sentence)
(bind-key "M-X" 'mark-sexp)
(bind-key "M-H" 'mark-paragraph)
(bind-key "M-D" 'mark-defun)

(bind-key "M-g c" 'goto-char)
(bind-key "M-g l" 'goto-line)

;;;; obolseted by aggressive-indent
;; (defun delete-indentation-forward ()
;;   (interactive)
;;   (delete-indentation t))

;;;_  . M-C-

(bind-key "<C-M-backspace>" 'backward-kill-sexp)

;;;_ , ctl-x-map

;;;_  . C-x

(defun dired-here ()
  "Make an up-to-date Dired buffer for the current location."
  (interactive)
  (dired ".")
  (revert-buffer))

(global-set-key (kbd "C-x d") 'dired-here)

;;; I don't use rectangles enough to justify these bindings
;;
;; (defvar edit-rectangle-origin)
;; (defvar edit-rectangle-saved-window-config)

;; (defun edit-rectangle (&optional start end)
;;   (interactive "r")
;;   (let ((strs (delete-extract-rectangle start end))
;;         (mode major-mode)
;;         (here (copy-marker (min (mark) (point)) t))
;;         (config (current-window-configuration)))
;;     (with-current-buffer (generate-new-buffer "*Rectangle*")
;;       (funcall mode)
;;       (set (make-local-variable 'edit-rectangle-origin) here)
;;       (set (make-local-variable 'edit-rectangle-saved-window-config) config)
;;       (local-set-key (kbd "C-c C-c") #'restore-rectangle)
;;       (mapc #'(lambda (x) (insert x ?\n)) strs)
;;       (goto-char (point-min))
;;       (pop-to-buffer (current-buffer)))))

;; (defun restore-rectangle ()
;;   (interactive)
;;   (let ((content (split-string (buffer-string) "\n"))
;;         (origin edit-rectangle-origin)
;;         (config edit-rectangle-saved-window-config))
;;     (with-current-buffer (marker-buffer origin)
;;       (goto-char origin)
;;       (insert-rectangle content))
;;     (kill-buffer (current-buffer))
;;     (set-window-configuration config)))

;; (bind-key "C-x D" #'edit-rectangle)
;; (bind-key "C-x d" #'delete-whitespace-rectangle)

(bind-key "C-x F" #'set-fill-column)
(bind-key "C-x t" #'toggle-truncate-lines)

(bind-key "C-x v H" #'vc-region-history)
(bind-key "C-x K" #'delete-current-buffer-file)

;;;_  . C-x C-

(defun duplicate-line ()
  "Duplicate the line containing point."
  (interactive)
  (save-excursion
    (let (line-text)
      (goto-char (line-beginning-position))
      (let ((beg (point)))
        (goto-char (line-end-position))
        (setq line-text (buffer-substring beg (point))))
      (if (eobp)
          (insert ?\n)
        (forward-line))
      (open-line 1)
      (insert line-text))))

(bind-key "C-x C-d" 'duplicate-line)
(bind-key "C-x C-y" 'join-line)
(bind-key "C-x C-e" 'pp-eval-last-sexp)
(bind-key "C-x C-n" 'next-line)

(defun find-alternate-file-with-sudo ()
  "Find file via elevated privileges."
  (interactive)
  (find-alternate-file (concat "/sudo::" (buffer-file-name))))

(bind-key "C-x C-v" 'find-alternate-file-with-sudo)

;;;_  . C-x M-

(bind-key "C-x M-n" 'set-goal-column)

(defun refill-paragraph (arg)
  "Refill paragraph to column ARG."
  (interactive "*P")
  (let ((fun (if (memq major-mode '(c-mode c++-mode go-mode))
                 'c-fill-paragraph
               (or fill-paragraph-function
                   'fill-paragraph)))
        (width (if (numberp arg) arg))
        prefix beg end)
    (forward-paragraph 1)
    (setq end (copy-marker (- (point) 2)))
    (forward-line -1)
    (let ((b (point)))
      (skip-chars-forward "^A-Za-z0-9`'\"(")
      (setq prefix (buffer-substring-no-properties b (point))))
    (backward-paragraph 1)
    (if (eolp)
        (forward-char))
    (setq beg (point-marker))
    (delete-horizontal-space)
    (while (< (point) end)
      (delete-indentation 1)
      (end-of-line))
    (let ((fill-column (or width fill-column))
          (fill-prefix prefix))
      (if prefix
          (setq fill-column
                (- fill-column (* 2 (length prefix)))))
      (funcall fun nil)
      (goto-char beg)
      (insert prefix)
      (funcall fun nil))
    (goto-char (+ end 2))))

(bind-key "C-x M-q" 'refill-paragraph)

(defun endless/fill-or-unfill (count)
  "`fill-paragraph', with COUNT as prefix argument, but unfill if used twice."
  (interactive "P")
  (let ((fill-column
         (if count
             (prefix-numeric-value count)
           (if (eq last-command 'endless/fill-or-unfill)
               (progn (setq this-command nil)
                      (point-max))
             fill-column))))
    (fill-paragraph)))

(global-set-key [remap fill-paragraph]
                #'endless/fill-or-unfill)

;;;_ , mode-specific-map

;;;_  . C-c

(bind-key "C-c <tab>" 'ff-find-other-file)
(bind-key "C-c SPC" 'just-one-space)

(defvar insert-and-counting--index 1)
(defvar insert-and-counting--expr nil)

(defun insert-and-counting (&optional index expr)
  "Insert numbers starting from INDEX according to EXPR."
  (interactive
   (if (or current-prefix-arg
           (not insert-and-counting--expr))
       (list (setq insert-and-counting--index
                   (prefix-numeric-value current-prefix-arg))
             (setq insert-and-counting--expr
                   (eval-expr-read-lisp-object-minibuffer "Pattern: ")))
     (list (setq insert-and-counting--index
                 (1+ insert-and-counting--index))
           insert-and-counting--expr)))
  (let ((n insert-and-counting--index))
    (eval expr)))

(bind-key "C-c #" #'insert-and-counting)

(bind-key "C-c C-+" #'text-scale-decrease)
(bind-key "C-c C--" #'text-scale-increase)

;; inspired by Erik Naggum's `recursive-edit-with-single-window'
(defmacro recursive-edit-preserving-window-config (body)
  "*Return a command that enters a recursive edit after executing BODY.
Upon exiting the recursive edit (with\\[exit-recursive-edit] (exit)
 or \\[abort-recursive-edit] (abort)), restore window configuration
 in current frame."
  `(lambda ()
     "See the documentation for `recursive-edit-preserving-window-config'."
     (interactive)
     (save-window-excursion
       ,body
       (recursive-edit))))

(bind-key "C-c 0"
          (recursive-edit-preserving-window-config (delete-window)))
(bind-key "C-c 1"
          (recursive-edit-preserving-window-config
           (if (one-window-p 'ignore-minibuffer)
               (error "Current window is the only window in its frame")
             (delete-other-windows))))

(defun delete-current-line (&optional arg)
  "Delete ARG (default 1) lines."
  (interactive "p")
  (let ((here (point)))
    (beginning-of-line)
    (kill-line arg)
    (goto-char here)))

(bind-key "C-c d" 'delete-current-line)

(bind-key "C-c e E" 'elint-current-buffer)

(defun do-eval-buffer ()
  "Eval current buffer."
  (interactive)
  (call-interactively 'eval-buffer)
  (message "Buffer has been evaluated"))

(bind-keys :prefix-map my-lisp-devel-map
           :prefix "C-c e"
           ("E" . elint-current-buffer)
           ("b" . do-eval-buffer)
           ("c" . cancel-debug-on-entry)
           ("d" . debug-on-entry)
           ("e" . toggle-debug-on-error)
           ("f" . emacs-lisp-byte-compile-and-load)
           ("j" . emacs-lisp-mode)
           ("l" . find-library)
           ("r" . do-eval-region)
           ("s" . scratch)
           ("z" . byte-recompile-directory))

(bind-key "C-c f" 'flush-lines)
(bind-key "C-c k" 'keep-lines)

;; (eval-when-compile
;;   (defvar emacs-min-top)
;;   (defvar emacs-min-left)
;;   (defvar emacs-min-height)
;;   (defvar emacs-min-width))

;; (defun emacs-min ()
;;   "Switch to minimal window size."
;;   (interactive)
;;   (set-frame-parameter (selected-frame) 'fullscreen nil)
;;   (set-frame-parameter (selected-frame) 'vertical-scroll-bars nil)
;;   (set-frame-parameter (selected-frame) 'horizontal-scroll-bars nil)
;;   (set-frame-parameter (selected-frame) 'top emacs-min-top)
;;   (set-frame-parameter (selected-frame) 'left emacs-min-left)
;;   (set-frame-parameter (selected-frame) 'height emacs-min-height)
;;   (set-frame-parameter (selected-frame) 'width emacs-min-width))

;; ;; (if window-system
;; ;;     (add-hook 'after-init-hook 'emacs-min))

;; (defun emacs-max ()
;;   "Switch to maximized window size."
;;   (interactive)
;;   (if t
;;       (progn
;;         (set-frame-parameter (selected-frame) 'fullscreen 'fullboth)
;;         (set-frame-parameter (selected-frame) 'vertical-scroll-bars nil)
;;         (set-frame-parameter (selected-frame) 'horizontal-scroll-bars nil))
;;     (set-frame-parameter (selected-frame) 'top 26)
;;     (set-frame-parameter (selected-frame) 'left 2)
;;     (set-frame-parameter (selected-frame) 'width
;;                          (floor (/ (float (x-display-pixel-width)) 9.15)))
;;     (if (= 1050 (x-display-pixel-height))
;;         (set-frame-parameter (selected-frame) 'height
;;                              (if (>= emacs-major-version 24)
;;                                  66
;;                                55))
;;       (set-frame-parameter (selected-frame) 'height
;;                            (if (>= emacs-major-version 24)
;;                                75
;;                              64)))))

;; (defun emacs-toggle-size ()
;;   "Toggle between min and max widow sizes."
;;   (interactive)
;;   (if (> (cdr (assq 'width (frame-parameters))) 100)
;;       (emacs-min)
;;     (emacs-max)))

;; (bind-key "C-c m" 'emacs-toggle-size)

(defcustom user-initials nil
  "*Initials of this user."
  :set
  #'(lambda (symbol value)
      (if (fboundp 'font-lock-add-keywords)
          (mapc
           #'(lambda (mode)
               (font-lock-add-keywords
                mode (list (list (concat "\\<\\(" value " [^:\n]+\\):")
                                 1 font-lock-warning-face t))))
           '(c-mode c++-mode emacs-lisp-mode lisp-mode
                    python-mode perl-mode java-mode groovy-mode)))
      (set symbol value))
  :type 'string
  :group 'mail)

(defun insert-user-timestamp ()
  "Insert a quick timestamp using the value of `user-initials'."
  (interactive)
  (insert (format "%s (%s): " user-initials
                  (format-time-string "%Y-%m-%d" (current-time)))))

(bind-key "C-c i n" 'insert-user-timestamp)
(bind-key "C-c o" 'customize-option)
(bind-key "C-c O" 'customize-group)
(bind-key "C-c F" #'customize-face)

(bind-key "C-c q" 'fill-region)
(bind-key "C-c r" 'replace-regexp)
(bind-key "C-c s" 'replace-string)
(bind-key "C-c u" 'rename-uniquely)
(bind-key "M-SPC" 'cycle-spacing)

(autoload 'auth-source-search "auth-source")

(defun tinify-url (url)
  "Shortnen URL."
  (interactive "sURL to shorten: ")
  (let* ((api-login "jwiegley")
         (api-key
          (funcall
           (plist-get
            (car (auth-source-search :host "api.j.mp" :user api-login
                                     :type 'netrc :port 80))
            :secret))))
    (cl-flet ((message (&rest ignore)))
      (with-current-buffer
          (let ((query
                 (format "format=txt&longUrl=%s&login=%s&apiKey=%s"
                         (url-hexify-string url) api-login api-key)))
            (url-retrieve-synchronously
             (concat "http://api.j.mp/v3/shorten?" query)))
        (goto-char (point-min))
        (re-search-forward "^$")
        (prog1
            (kill-new (buffer-substring (1+ (point)) (1- (point-max))))
          (kill-buffer (current-buffer)))))))

(bind-key "C-c U" 'tinify-url)

(defun view-clipboard ()
  "Put clipboard contents in special buffer."
  (interactive)
  (delete-other-windows)
  (switch-to-buffer "*Clipboard*")
  (let ((inhibit-read-only t))
    (erase-buffer)
    (clipboard-yank)
    (goto-char (point-min))
    (html-mode)
    (view-mode)))

(bind-key "C-c V" 'view-clipboard)
(bind-key "C-c z" 'clean-buffer-list)

(bind-key "C-c [" 'align-regexp)

(use-package comment-line-or-region
  :straight nil
  :unless noninteractive
  :preface
  (defun endless/comment-line-or-region (n)
    "Comment or uncomment current line and leave point after it.
With positive prefix, apply to N lines including current one.
With negative prefix, apply to -N lines above.
If region is active, apply to active region instead."
    (interactive "p")
    (if (use-region-p)
        (comment-or-uncomment-region
         (region-beginning) (region-end))
      (let ((range
             (list (line-beginning-position)
                   (goto-char (line-end-position n)))))
        (comment-or-uncomment-region
         (apply #'min range)
         (apply #'max range)))
      (forward-line 1)
      (back-to-indentation)))

  :bind (:map prog-mode-map
              ("C-c ;" . endless/comment-line-or-region)))

(defun vlc-play-sound (file)
  "Play a sound FILE with vlc, but do it asynchronously."
  (start-process-shell-command "cvlc"
                               nil
                               (concat "cvlc --play-and-stop " file)))

;;;_  . C-c C-

(defun delete-to-end-of-buffer ()
  "Kill from point to eodn of buffer."
  (interactive)
  (kill-region (point) (point-max)))

(bind-key "C-c C-z" 'delete-to-end-of-buffer)

;;;_  . C-c M-

(defun unfill-paragraph ()
  "Unfill."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil t)))

(defun unfill-region (beg end)
  "Unfills from BEG to END."
  (interactive "r")
  (setq end (copy-marker end))
  (save-excursion
    (goto-char beg)
    (while (< (point) end)
      (unfill-paragraph)
      (forward-paragraph))))

(bind-key "C-c M-q" 'unfill-paragraph)

;;;_ , help-map

(defvar lisp-find-map)
(define-prefix-command 'lisp-find-map)

(bind-key "C-h e" 'lisp-find-map)

;;;_  . C-h e

(bind-key "C-h e c" 'finder-commentary)
(bind-key "C-h e e" 'view-echo-area-messages)
(bind-key "C-h e f" 'find-function)
(bind-key "C-h e F" 'find-face-definition)
(bind-key "C-h e i" 'info-apropos)
(bind-key "C-h e k" 'find-function-on-key)
(bind-key "C-h e l" 'find-library)

(defvar lisp-modes  '(emacs-lisp-mode
                      inferior-emacs-lisp-mode
                      ielm-mode
                      lisp-mode
                      inferior-lisp-mode
                      lisp-interaction-mode
                      slime-repl-mode))

(defvar lisp-mode-hooks
  (mapcar (function
           (lambda (mode)
             (intern
              (concat (symbol-name mode) "-hook"))))
          lisp-modes))

;; (switch-to-buffer "*scratch*")
;; (org-mode)

(defun scratch ()
  "Switch to scratch buffer."
  (interactive)
  (let ((current-mode major-mode))
    (switch-to-buffer-other-window (get-buffer-create "*scratch*"))
    (goto-char (point-min))
    (when (looking-at ";")
      (forward-line 4)
      (delete-region (point-min) (point)))
    (goto-char (point-max))
    (if (memq current-mode lisp-modes)
        (funcall current-mode))))

(bind-key "C-h e s" 'scratch)
(bind-key "C-h e v" 'find-variable)
(bind-key "C-h e V" 'apropos-value)

;;;_. Packages

;;;_ , cc-mode

(use-package cc-mode
  :mode (("\\.h\\(h?\\|xx\\|pp\\)\\'" . c++-mode)
         ("\\.m\\'" . c-mode)
         ("\\.mm\\'" . c++-mode))
  :bind (:map c++-mode-map
              ("<" . self-insert-command)
              (">" . self-insert-command))
  :bind (:map c-mode-base-map
              ("#" . self-insert-command)
              ("{" . self-insert-command)
              ("}" . self-insert-command)
              ("/" . self-insert-command)
              ("*" . self-insert-command)
              (";" . self-insert-command)
              ("," . self-insert-command)
              (":" . self-insert-command)
              ("(" . self-insert-command)
              (")" . self-insert-command)
              ("<return>" . newline-and-indent)
              ("M-q" . c-fill-paragraph)
              ("M-j"))
  :preface
  (defun my-c-mode-common-hook ()
    (require 'flycheck)
    ;; (flycheck-define-checker
    ;;  c++-ledger
    ;;  "A C++ syntax checker for the Ledger project specifically."
    ;;  :command ("ninja"
    ;;            "-C"
    ;;            (eval (expand-file-name "~/Products/ledger"))
    ;;            (eval (concat "src/CMakeFiles/libledger.dir/"
    ;;                          (file-name-nondirectory (buffer-file-name))
    ;;                          ".o")))
    ;;  :error-patterns
    ;;  ((error line-start
    ;;          (message "In file included from") " " (or "<stdin>" (file-name))
    ;;          ":" line ":" line-end)
    ;;   (info line-start (or "<stdin>" (file-name)) ":" line ":" column
    ;;         ": note: " (optional (message)) line-end)
    ;;   (warning line-start (or "<stdin>" (file-name)) ":" line ":" column
    ;;            ": warning: " (optional (message)) line-end)
    ;;   (error line-start (or "<stdin>" (file-name)) ":" line ":" column
    ;;          ": " (or "fatal error" "error") ": " (optional (message)) line-end))
    ;;  :error-filter
    ;;  (lambda (errors)
    ;;    (let ((errors (flycheck-sanitize-errors errors)))
    ;;      (dolist (err errors)
    ;;        ;; Clang will output empty messages for #error/#warning pragmas
    ;;        ;; without messages. We fill these empty errors with a dummy message
    ;;        ;; to get them past our error filtering
    ;;        (setf (flycheck-error-message err)
    ;;              (or (flycheck-error-message err) "no message")))
    ;;      (flycheck-fold-include-levels errors "In file included from")))
    ;;  :modes c++-mode
    ;;  :next-checkers ((warning . c/c++-cppcheck)))

    (flycheck-mode 1)
    ;; (flycheck-select-checker 'c++-ledger)
    (setq-local flycheck-check-syntax-automatically nil)
    (setq-local flycheck-highlighting-mode nil)

    (set (make-local-variable 'parens-require-spaces) nil)

    (let ((bufname (buffer-file-name)))
      (when bufname
        (cond
         ((string-match "/ledger/" bufname)
          (c-set-style "ledger"))
         ((string-match "/edg/" bufname)
          (c-set-style "edg"))
         (t
          (c-set-style "clang")))))

    (font-lock-add-keywords
     'c++-mode '(("\\<\\(assert\\|DEBUG\\)(" 1 font-lock-warning-face t))))

  :hook (c-mode-common . my-c-mode-common-hook)
  :config
  (add-to-list
   'c-style-alist
   '("edg"
     (indent-tabs-mode . nil)
     (c-basic-offset . 2)
     (c-comment-only-line-offset . (0 . 0))
     (c-hanging-braces-alist
      . ((substatement-open before after)
         (arglist-cont-nonempty)))
     (c-offsets-alist
      . ((statement-block-intro . +)
         (knr-argdecl-intro . 5)
         (substatement-open . 0)
         (substatement-label . 0)
         (label . 0)
         (case-label . +)
         (statement-case-open . 0)
         (statement-cont . +)
         (arglist-intro . +)
         (arglist-close . +)
         (inline-open . 0)
         (brace-list-open . 0)
         (topmost-intro-cont
          . (first c-lineup-topmost-intro-cont
                   c-lineup-gnu-DEFUN-intro-cont))))
     (c-special-indent-hook . c-gnu-impose-minimum)
     (c-block-comment-prefix . "")))

  (add-to-list
   'c-style-alist
   '("ledger"
     (indent-tabs-mode . nil)
     (c-basic-offset . 2)
     (c-comment-only-line-offset . (0 . 0))
     (c-hanging-braces-alist
      . ((substatement-open before after)
         (arglist-cont-nonempty)))
     (c-offsets-alist
      . ((statement-block-intro . +)
         (knr-argdecl-intro . 5)
         (substatement-open . 0)
         (substatement-label . 0)
         (label . 0)
         (case-label . 0)
         (statement-case-open . 0)
         (statement-cont . +)
         (arglist-intro . +)
         (arglist-close . +)
         (inline-open . 0)
         (brace-list-open . 0)
         (topmost-intro-cont
          . (first c-lineup-topmost-intro-cont
                   c-lineup-gnu-DEFUN-intro-cont))))
     (c-special-indent-hook . c-gnu-impose-minimum)
     (c-block-comment-prefix . "")))

  (add-to-list
   'c-style-alist
   '("clang"
     (indent-tabs-mode . nil)
     (c-basic-offset . 2)
     (c-comment-only-line-offset . (0 . 0))
     (c-hanging-braces-alist
      . ((substatement-open before after)
         (arglist-cont-nonempty)))
     (c-offsets-alist
      . ((statement-block-intro . +)
         (knr-argdecl-intro . 5)
         (substatement-open . 0)
         (substatement-label . 0)
         (label . 0)
         (case-label . 0)
         (statement-case-open . 0)
         (statement-cont . +)
         (arglist-intro . +)
         (arglist-close . +)
         (inline-open . 0)
         (brace-list-open . 0)
         (topmost-intro-cont
          . (first c-lineup-topmost-intro-cont
                   c-lineup-gnu-DEFUN-intro-cont))))
     (c-special-indent-hook . c-gnu-impose-minimum)
     (c-block-comment-prefix . ""))))

;;;_, abbrev

(use-package abbrev
  ;; internal
  :straight nil
  :diminish t
  :preface
  (defun define-abbrev-sedlike (text)
    "Search TEXT for sedlike s/./,/ and add abbrev to mode table."
    (interactive "sSedlike Expression? ")
    (when (string-match "^[Ss]/\\([^/]*\\)/\\([^/]*\\)/" text)
      (define-abbrev local-abbrev-table
        (match-string 1 text)
        (match-string 2 text))))
  :hook
  ((text-mode prog-mode erc-mode LaTeX-mode) . #'abbrev-mode)
  (expand-load
   . (lambda ()
       (add-hook 'expand-expand-hook #'indent-according-to-mode)
       (add-hook 'expand-jump-hook #'indent-according-to-mode)))
  :custom
  (abbrev-file-name (emacs-path "abbrevs.el"))
  :config
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file)))

;;;_ , ace-jump-mode

(use-package ace-jump-mode
  :defer t
  :chords (("jj" . ace-jump-char-mode)
           ("jk" . ace-jump-word-mode)
           ("jl" . ace-jump-line-mode)
           ("jh" . ace-jump-mode-pop-mark))
  :bind (("C-c C-0" . ace-jump-mode)
         ("C-x SPC" . ace-jump-mode-pop-mark))
  :config
  (progn
    (ace-jump-mode-enable-mark-sync)))

;;;_ , ace-mc

(use-package ace-mc
  :after (multiple-cursors)
  :bind (("C-)" . ace-mc-add-multiple-cursors)
         ("C-M-)" . ace-mc-add-single-cursor)))

;;;_ , ace-window

(use-package ace-window
  :chords ("jw" . ace-jump-char-mode)
  :bind ("M-i" . ace-window))

;;;_ , aggressive-indent

(use-package aggressive-indent
  :config
  (aggressive-indent-global-mode +1))

;;;_ , alert

(use-package alert)

(use-package alert-play
  :disabled t
  :after alert
  :load-path "/home/emacs/alert-play"
  :custom
  (alert-play-default-playable "/home/emacs/imsounds/huh.wav")
  (alert-play-volume 0.2)
  (alert-play-command "play"))

;;;_ , ascii

(use-package ascii
  :bind ("C-c e A" . ascii-toggle)
  :commands (ascii-on ascii-off ascii-toggle)
  :preface
  (defun ascii-toggle ()
    (interactive)
    (if ascii-display
        (ascii-off)
      (ascii-on))))

;;;_ , archive-region

(use-package archive-region
  :commands kill-region-or-archive-region
  :bind ("C-w" . kill-region-or-archive-region))

(use-package atomic-chrome
  :custom
  (atomic-chrome-url-major-mode-alist
   '(("atlassian\\.net" . jira-markup-mode)
     ("reddit\\.com" . markdown-mode)
     ("github\\.com" . gfm-mode)
     ("redmine" . textile-mode))
   "Major modes for URLs.")
  (atomic-chrome-server-ghost-text-port (+ 4000 user-number))
  :config
  (atomic-chrome-start-server))

(use-package auth-source-pass
  :disabled t
  :preface
  (defvar auth-source-pass--cache (make-hash-table :test #'equal))

  (defun auth-source-pass--reset-cache ()
    (setq auth-source-pass--cache (make-hash-table :test #'equal)))

  (defun auth-source-pass--read-entry (entry)
    "Return a string with the file content of ENTRY."
    (run-at-time 45 nil #'auth-source-pass--reset-cache)
    (let ((cached (gethash entry auth-source-pass--cache)))
      (or cached
          (puthash
           entry
           (with-temp-buffer
             (insert-file-contents (expand-file-name
                                    (format "%s.gpg" entry)
                                    (getenv "PASSWORD_STORE_DIR")))
             (buffer-substring-no-properties (point-min) (point-max)))
           auth-source-pass--cache))))

  (defun auth-source-pass-entries ()
    "Return a list of all password store entries."
    (let ((store-dir (getenv "PASSWORD_STORE_DIR")))
      (mapcar
       (lambda (file) (file-name-sans-extension (file-relative-name file store-dir)))
       (directory-files-recursively store-dir "\.gpg$"))))
  :config
  (auth-source-pass-enable))

(use-package autorevert
  :config
  (global-auto-revert-mode t))

(use-package autoinsert
  :straight nil
  :defer t
  :bind (("C-c i a" . auto-insert))
  :config
  (auto-insert-mode +1))

;;;_ , avy

(use-package avy
  :bind ("C-." . avy-goto-char-timer)
  :custom
  (avy-case-fold-search t)
  (avy-keys '(97 111 101 117 105 100 104 116 110 115))
  (avy-timeout-seconds 0.3)
  :functions (avy-setup-default)
  :preface
  (defun avy-action-kill-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (kill-whole-line))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

  (defun avy-action-copy-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end)
          (bounds-of-thing-at-point 'line)
        (copy-region-as-kill start end)))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

  (defun avy-action-yank-whole-line (pt)
    (avy-action-copy-whole-line pt)
    (save-excursion (yank))
    t)

  (defun avy-action-teleport-whole-line (pt)
    (avy-action-kill-whole-line pt)
    (save-excursion (yank)) t)

  (defun avy-action-mark-to-char (pt)
    (activate-mark)
    (goto-char pt))
  :config
  (avy-setup-default)

  (setf (alist-get ?k avy-dispatch-alist) 'avy-action-kill-stay
        (alist-get ?K avy-dispatch-alist) 'avy-action-kill-whole-line)

  (setf (alist-get ?y avy-dispatch-alist) 'avy-action-yank
        (alist-get ?w avy-dispatch-alist) 'avy-action-copy
        (alist-get ?W avy-dispatch-alist) 'avy-action-copy-whole-line
        (alist-get ?Y avy-dispatch-alist) 'avy-action-yank-whole-line)

  (setf (alist-get ?t avy-dispatch-alist) 'avy-action-teleport
        (alist-get ?T avy-dispatch-alist) 'avy-action-teleport-whole-line)

  (setf (alist-get ?  avy-dispatch-alist) 'avy-action-mark-to-char))

(use-package avy-embark
  :straight nil
  :no-require t
  :after (avy embark)
  :preface
  (defun avy-action-embark (pt)
    (require 'embark
             (unwind-protect
                 (save-excursion
                   (goto-char pt)
                   (embark-act))
               (select-window
                (cdr (ring-ref avy-ring 0))))
             t))
  :config
  (setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark))

(use-package avy-flyspell
  :straight nil
  :no-require t
  :after avy
  :functions (flyspell-auto-correct-word)
  :preface
  (defun avy-action-flyspell (pt)
    (save-excursion
      (goto-char pt)
      (when (require 'flyspell nil t)
        (flyspell-auto-correct-word)))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)
  :config
  (setf (alist-get ?\; avy-dispatch-alist) 'avy-action-flyspell))

(use-package avy-zap
  :straight nil
  :bind (("M-z" . avy-zap-up-to-char-dwim)
         ("M-Z" . avy-zap-to-char-dwim)))

;;;_ , auto-yasnippet

(use-package auto-yasnippet
  :after yasnippet
  :bind (("C-c y a" . aya-create)
         ("C-c y e" . aya-expand)
         ("C-c y o" . aya-open-line)))

;;;_ , backup-each-save

(use-package backup-each-save
  :commands backup-each-save
  :preface
  (defun my-make-backup-file-name (file)
    (make-backup-file-name-1 (expand-file-name (file-truename file))))

  (defun backup-each-save-filter (filename)
    (not (string-match
          (concat "\\(^/tmp\\|\\.emacs\\.d/data\\(-alt\\)?/"
                  "\\|\\.newsrc\\(\\.eld\\)?\\|"
                  "\\(archive/sent/\\|recentf\\`\\)\\)")
          filename)))

  (defun my-dont-backup-files-p (filename)
    (unless (string-match filename "\\(archive/sent/\\|recentf\\`\\)")
      (normal-backup-enable-predicate filename)))

  :hook after-save
  :config
  (setq backup-each-save-filter-function 'backup-each-save-filter
        backup-enable-predicate 'my-dont-backup-files-p))

(use-package backup-walker
  :commands backup-walker-start)

(use-package bazel)

;;;_ , bbdb

(use-package bbdb
  :bind ("M-B" . bbdb-mode))

;;;_ , beacon

(use-package beacon
  :config
  (progn
    (beacon-mode 1)
    (setq beacon-push-mark 35)
    (setq beacon-color "#666600")))

;;;_ , beginend

(use-package beginend
  ;; BULK-ENSURE :ensure t
  :diminish
  :config
  (beginend-global-mode +1))

(use-package blamer
  :after posframe
  :straight (blamer :host github :repo "artawower/blamer.el")
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                   :background nil
                   :height 70
                   :italic t))))

;;;_ , bm

;; not compatible with ivy

(use-package bm
  :bind (("C-c b b" . bm-toggle)
         ("C-c b n" . bm-next)
         ("C-c b p" . bm-previous))
  :commands (bm-repository-load
             bm-buffer-save
             bm-buffer-save-all
             bm-buffer-restore)
  :hook
  (after-init        . bm-repository-load)
  (find-file         . bm-buffer-restore)
  (after-revert      . bm-buffer-restore)
  (kill-buffer       . bm-buffer-save)
  (after-save        . bm-buffer-save)
  (vc-before-checkin . bm-buffer-save)
  (kill-emacs        . (lambda ()
                         (bm-buffer-save-all)
                         (bm-repository-save)))
  :custom
  (bm-buffer-persistence t)
  (bm-cycle-all-buffers t)
  (bm-highlight-style 'bm-highlight-only-fringe)
  (bm-in-lifo-order t)
  (bm-repository-file (user-data "bm-repository")))

(use-package bookmark
  :straight nil
  :defer t
  :bind
  ("<f4>" . (lambda () (interactive) (bookmark-set "SAVED")))
  ("<f1>" . (lambda () (interactive) (bookmark-jump "SAVED")))
  :custom
  (bookmark-default-file (emacs-path "bookmarks")))

(use-package browse-url
  :straight nil
  :defer t
  :config
  (setq browse-url-generic-program (or (executable-find "firefox")
                                       (executable-find "chrome"))))

(use-package browse-kill-ring
  :commands browse-kill-ring)

(use-package calc
  :straight nil
  :defer t
  :custom
  (math-additional-units
   '((GiB "1024 * MiB" "Gibi Byte")
     (MiB "1024 * KiB" "Mebi Byte")
     (KiB "1024 * B" "Kibi Byte")
     (Gib "1024 * Mib" "Gibi Bit")
     (Mib "1024 * Kib" "Mebi Bit")
     (Kib "1024 * b" "Kibi Bit")
     (GB "1000 * MB" "Giga Byte")
     (MB "1000 * KB" "Mega Byte")
     (KB "1000 * B" "Kilo Byte")
     (Gb "1000 * Mb" "Giga Bit")
     (Mb "1000 * Kb" "Mega Bit")
     (Kb "1000 * b" "Kilo Bit")
     (B nil "Byte")
     (b "B / 8" "Bit")))
  (math-units-table nil))

(use-package calendar
  :straight nil
  :custom
  (calendar-mark-holidays-flag t)
  (calendar-date-style 'iso)
  (diary-file (emacs-path "diary")))

(use-package cal-dst
  :straight nil
  :custom
  (calendar-daylight-time-zone-name "PDT")
  (calendar-standard-time-zone-name "PST")
  (calendar-time-zone -480)
  :init
  (setenv "TZ" "PST8PDT"))

;;;_ , captain

(use-package captain
  :diminish "🚢"
  :config
  (add-hook 'prog-mode-hook
            (lambda ()
              (setq captain-predicate (
                                       lambda ()
                                       (nth 8 (syntax-ppss (point)))))))

  (add-hook 'text-mode-hook
            (lambda ()
              (setq captain-predicate (lambda () t))))

  (add-hook 'slack-message-buffer-mode-hook
            (lambda ()
              (setq captain-predicate (lambda () t))))

  (add-hook 'erc-mode-hook
            (lambda ()
              (setq captain-predicate (lambda () (> (length (word-at-point)) 1)))))

  (add-hook
   'org-mode-hook
   (lambda ()
     (setq captain-predicate
           (lambda () (not (org-in-src-block-p))))))
  (global-captain-mode +1))

(use-package shell-maker
  :straight (:host github :repo "xenodium/chatgpt-shell"
                   :files ("shell-maker.el")))

(use-package chatgpt-shell
  :requires shell-maker
  :custom
  (chatgpt-shell-openai-key
   (auth-source-pick-first-password :host "api.openai.com"))
  :straight (:host github :repo "xenodium/chatgpt-shell"
                   :files ("chatgpt-shell.el")))

(use-package chatgpt-shell
  :disabled t                           ; when using =package=
  :ensure t
  :custom
  ((chatgpt-shell-api-url-base "https://api.chatgpt.domain.com")
   (chatgpt-shell-openai-key
    (lambda ()
      ;; Here the openai-key should be the proxy service key.
      (auth-source-pass-get 'secret "openai-key")))))

(use-package copilot
  :disabled t
  :straight (:type git :host github :repo zerofx/copilot.el :branch master)
  :hook (prog-mode . copilot-mode)
  :init
  (my/toggle-map
   :keymaps 'override
   :states '(normal insert motion)
   "g" #'copilot-mode))


;;;_ , cider

(use-package cider
  :after clojure-mode
  :unless noninteractive
  :custom
  ;; (cider-repl-pop-to-buffer-on-connect nil)
  ;; (nrepl-hide-special-buffers t)
  ;; (cider-popup-stacktraces-in-repl t)
  (cider-repl-history-file "~/.emacs.d/nrepl-history")
  (cider-repl-pop-to-buffer-on-connect nil)
  (cider-auto-select-error-buffer nil)
  (cider-prompt-save-file-on-load nil)
  (cider-repl-display-help-banner nil)
  (cider-repl-use-pretty-printing t)
  ;; (cider-refresh-before-fn "reloaded.repl/suspend")
  ;; (cider-refresh-after-fn "reloaded.repl/resume")
  ;; (cider-cljs-lein-repl "(do (reloaded.repl/go) (user/cljs-repl))")
  ;; (cider-prompt-for-symbol nil)

  :hook (cider-repl-mode . (lambda ()
                             (aggressive-indent-mode -1)
                             (lispy-mode +1)))
  :config)

;; (defun tdd-test ()
;;   "Thin wrapper around `cider-test-run-tests'."
;;   (when (cider-connected-p)
;;     (cider-test-run-tests nil))

;;   (define-minor-mode tdd-mode
;;     "Run all tests whenever a file is saved."
;;     nil " TDD" nil
;;     (if tdd-mode
;;         (add-hook 'after-save-hook #'tdd-test nil 'local)
;;       (remove-hook 'after-save-hook #'tdd-test 'local)))

;;   (add-hook 'cider-mode-hook #'tdd-mode)
;;   (add-hook 'cider-repl-mode-hook #'tdd-mode)

;;   )
;; :hook (cider-repl-mode . #'lispy-mode)
;; :hook (cider-repl-mode . #'sotclojure-mode)

;;;_ , clj-refactor

(use-package clj-refactor
  :unless noninteractive
  :after clojure-mode
  :diminish ""
  :init
  (add-hook 'clojure-mode-hook
            (lambda ()
              (clj-refactor-mode 1)
              (yas-minor-mode 1)
              (cljr-add-keybindings-with-prefix "C-c C-x")))
  :custom
  (cljr-warn-on-eval nil)
  (cljr-ignore-analyzer-errors t)
  :config
  (dolist (mapping '(("time" . "clj-time.core")
                     ("string" . "clojure.string")
                     ("http" . "clj-http.client")
                     ("json" . "cheshire.core")
                     ("async" . "clojure.core.async")))
    (add-to-list 'cljr-magic-require-namespaces mapping t)))

;;;_ , clojure-mode

(use-package clojure-mode
  ;; BULK-ENSURE :ensure t
  :hook (clojure-mode . (lambda () (add-hook 'after-save-hook 'check-parens nil t)))
  :mode (("\\.edn$" . clojure-mode)
         ("\\.clj$" . clojure-mode)))

(use-package clojure-essential-ref
  :after clojure-mode
  :bind (:map cider-mode-map
              ("C-h F" . clojure-essential-ref)
              :map cider-repl-mode-map
              ("C-h F" . clojure-essential-ref)))

(use-package clojure-essential-ref-nov
  :disabled t
  :init
  (setq clojure-essential-ref-nov-epub-path "~/Downloads/Clojure_The_Essential_Reference_v29_MEAP.epub"))

(use-package html-to-hiccup
  ;; BULK-ENSURE :ensure t
  :after clojure-mode
  :config
  (define-key clojure-mode-map (kbd "C-c M-h")
    'html-to-hiccup-convert-region))

(use-package clojure-snippets
  ;; BULK-ENSURE :ensure t
  :after yasnippet clojure-mode)

(use-package clojure-mode-extra-font-locking
  :after clojure-mode)

(use-package sotclojure
  ;; BULK-ENSURE :ensure t
  :after clojure-mode
  :config
  (sotclojure-turn-on-everywhere))

;;;_ , cmake-mode

(use-package cmake-mode
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'"         . cmake-mode)))

(use-package color-identifiers-mode
  :unless noninteractive
  :diminish color-identifiers-mode
  :custom
  (color-identifiers-coloring-method 'hash)
  :hook (after-init . (lambda () (global-color-identifiers-mode +1))))

;;;_ , color-moccur

(use-package color-moccur
  :commands (isearch-moccur isearch-all isearch-moccur-all)
  :bind (("M-s O" . moccur)
         :map isearch-mode-map
         ("M-o" . isearch-moccur)
         ("M-O" . isearch-moccur-all)))

(use-package command-log-mode
  :bind (("C-c e M" . command-log-mode)
         ("C-c e L" . clm/open-command-log-buffer)))

(use-package compile
  :straight nil
  :bind (("C-c c" . compile)
         ("M-O"   . show-compilation))
  :bind (:map compilation-mode-map
              ("z" . delete-window))
  :hook (compilation-filter . compilation-ansi-color-process-output)
  :custom
  (compilation-always-kill t)
  (compilation-ask-about-save nil)
  (compilation-context-lines 10)
  (compilation-scroll-output 'first-error)
  (compilation-skip-threshold 2)
  (compilation-window-height 100)
  :preface
  (defun show-compilation ()
    (interactive)
    (let ((it
           (catch 'found
             (dolist (buf (buffer-list))
               (when (string-match "\\*compilation\\*" (buffer-name buf))
                 (throw 'found buf))))))
      (if it
          (display-buffer it)
        (call-interactively 'compile))))

  (defun compilation-ansi-color-process-output ()
    (ansi-color-process-output nil)
    (set (make-local-variable 'comint-last-output-start)
         (point-marker))))

(use-package copy-as-format
  :bind (("C-c w m" . copy-as-format-markdown)
         ("C-c w s" . copy-as-format-slack)
         ("C-c w o" . copy-as-format-org-mode)
         ("C-c w r" . copy-as-format-rst)
         ("C-c w g" . copy-as-format-github)
         ("C-c w w" . copy-as-format))
  :custom
  (copy-as-format-default "markdown")
  :config
  (defun copy-as-format--org-mode (text _multiline)
    (format "#+begin_src %s\n%s\n#+end_src\n"
            (replace-regexp-in-string "-mode\\'" "" (symbol-name major-mode))
            text)))

(use-package corfu
  :bind (:map corfu-map
              ("TAB" . corfu-next)
              ([tab] . corfu-next)
              ("S-TAB" . corfu-previous)
              ([backtab] . corfu-previous)
              ([remap completion-at-point] . corfu-complete)
              ("RET" . corfu-complete-and-quit)
              ("<return>" . corfu-complete-and-quit))
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-preselect-first t)
  (corfu-scroll-margin 4)
  (corfu-quit-no-match t)
  (corfu-quit-at-boundary t)
  (corfu-max-width 100)
  (corfu-min-width 42)
  (corfu-count 9)
  (corfu-auto-prefix 2)                 ;start after 2 chars
  ;; should be configured in the `indent' package, but `indent.el'
  ;; doesn't provide the `indent' feature.
  (tab-always-indent 'complete)
  :config
  (defun corfu-complete-and-quit ()
    (interactive)
    (corfu-complete)
    (corfu-quit))
  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active) ; Useful if I ever use MCT
                (bound-and-true-p vertico--input))
      (setq-local corfu-auto nil) ; Ensure auto completion is disabled
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)
  :config
  (global-corfu-mode +1))

(use-package popon
  :straight (:type git
                   :host codeberg
                   :repo "akib/emacs-popon"
                   :branch "master")
  :unless (display-graphic-p))

(use-package corfu-terminal
  :straight (:type git
                   :host codeberg
                   :repo "akib/emacs-corfu-terminal"
                   :branch "master")
  :requires popon
  :unless (display-graphic-p)
  :config
  (corfu-terminal-mode +1))

(use-package elisp-mode-cape
  :no-require t
  :straight nil
  :after (cape elisp-mode)
  :hook (emacs-lisp-mode . my/setup-elisp)
  :preface
  (defun my/setup-elisp ()
    (setq-local completion-at-point-functions
                `(,(cape-super-capf
                    #'elisp-completion-at-point
                    #'cape-dabbrev)
                  cape-file)
                cape-dabbrev-min-length 5)))

(use-package cape
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :config
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)

  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)
  )

(use-package elisp-mode-cape
  :no-require t
  :straight nil
  :after (cape elisp-mode)
  :hook (emacs-lisp-mode . my/setup-elisp)
  :preface
  (defun my/setup-elisp ()
    (setq-local completion-at-point-functions
                `(,(cape-super-capf
                    #'elisp-completion-at-point
                    #'cape-dabbrev)
                  cape-file)
                cape-dabbrev-min-length 5)))

(use-package marginalia
  :after vertico
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :custom
  (marginalia-align 'right)
  (marginalia-annotators
   '(marginalia-annotators-heavy marginalia-annotators-light nil))
  (popwin:universal-display-config
   (marginalia-mode +1)))

(use-package embark
  :after vertico
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("M-." . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind ;; C-c bindings in `mode-specific-map'
  (("C-c M-x" . consult-mode-command)
   ("C-c h" . consult-history)
   ("C-c k" . consult-kmacro)
   ("C-c m" . consult-man)
   ("C-c i" . consult-info)
   ([remap Info-search] . consult-info)
   ;; C-x bindings in `ctl-x-map'
   ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
   ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
   ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
   ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
   ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
   ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
   ;; Custom M-# bindings for fast register access
   ("M-#" . consult-register-load)
   ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
   ("C-M-#" . consult-register)
   ;; Other custom bindings
   ("M-y" . consult-yank-pop)                ;; orig. yank-pop
   ;; M-g bindings in `goto-map'
   ("M-g e" . consult-compile-error)
   ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
   ("M-g g" . consult-goto-line)             ;; orig. goto-line
   ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
   ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
   ("M-g m" . consult-mark)
   ("M-g k" . consult-global-mark)
   ("M-g i" . consult-imenu)
   ("M-g I" . consult-imenu-multi)
   ;; M-s bindings in `search-map'
   ("M-s d" . consult-find)
   ("M-s D" . consult-locate)
   ("M-s g" . consult-grep)
   ("M-s G" . consult-git-grep)
   ("M-s r" . consult-ripgrep)
   ("C-c g" . consult-line)
   ("M-s L" . consult-line-multi)
   ("M-s k" . consult-keep-lines)
   ("M-s u" . consult-focus-lines)
   ;; Isearch integration
   ("M-s e" . consult-isearch-history)
   :map isearch-mode-map
   ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
   ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
   ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
   ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
   ;; Minibuffer history
   :map minibuffer-local-map
   ("M-s" . consult-history)                 ;; orig. next-matching-history-element
   ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)
  )

(use-package consult-dir
  :after consult
  :bind
  (:map vertico-map
        ("M-." . consult-dir)
        ("M-j" . consult-dir-jump-file))
  :custom
  (consult-dir-shadow-filenames nil)
  (consult-dir-sources '( consult-dir--source-bookmark
                          consult-dir--source-default
                          consult-dir--source-project
                          consult-dir--source-recentf))
  :config
  (dolist (map (list global-map minibuffer-local-filename-completion-map))
    (define-key map (kbd "C-x C-d") #'consult-dir)))

(use-package consult-project-extra
  :defer t)

(use-package consult-eglot
  :after consult eglot
  :defer t)

(use-package consult-projectile
  :straight (consult-projectile
             :type git
             :host gitlab
             :repo "OlMon/consult-projectile"
             :branch "master"))

(use-package copy-as-format
  :bind (("C-c w m" . copy-as-format-markdown)
         ("C-c w s" . copy-as-format-slack)
         ("C-c w o" . copy-as-format-org-mode)
         ("C-c w r" . copy-as-format-rst)
         ("C-c w g" . copy-as-format-github)
         ("C-c w w" . copy-as-format))
  :custom
  (copy-as-format-default "slack")
  :config
  (defun copy-as-format--org-mode (text _multiline)
    (format "#+begin_src %s\n%s\n#+end_src\n"
            (replace-regexp-in-string "-mode\\'" "" (symbol-name major-mode))
            text)))

;;;_ , crosshairs

(use-package crosshairs
  :unless (or install-run noninteractive)
  :bind ("M-o c" . crosshairs-mode))

(use-package ctrlf
  :config (ctrlf-mode +1))

(use-package current-word-highlight-mode
  :disabled t
  :unless (or install-run noninteractive))

;;;_ , css-eldoc

(use-package css-eldoc)

(use-package dabbrev
  :straight nil
  :bind ("C-M-/" . dabbrev-expand)
  :custom
  (dabbrev-case-fold-search nil)
  (dabbrev-case-replace nil)
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

(use-package deadgrep
  :bind ("M-s d" . deadgrep))

;;;_ , dedicated

(use-package dedicated
  :bind ("C-c W" . dedicated-mode))

(use-package default-text-scale
  :config)
(default-text-scale-mode +1)

(use-package devdocs
  :disabled t
  :straight nil
  :autoload (devdocs--installed-docs devdocs--available-docs)
  :bind (:map prog-mode-map
              ("M-<f1>" . devdocs-dwim)
              ("C-h D"  . devdocs-dwim))
  :init
  (defconst devdocs-major-mode-docs-alist
    '((c-mode          . ("c"))
      (c++-mode        . ("cpp"))
      (clojure-mode    . ("clojure-1.11"))
      (python-mode     . ("python~3.10" "python~2.7"))
      (ruby-mode       . ("ruby~3.1"))
      (rustic-mode     . ("rust"))
      (css-mode        . ("css"))
      (html-mode       . ("html"))
      (julia-mode      . ("julia~1.8"))
      (js-mode         . ("javascript" "jquery"))
      (js2-mode        . ("javascript" "jquery"))
      (emacs-lisp-mode . ("elisp")))
    "Alist of major-mode and docs.")

  (mapc
   (lambda (mode)
     (add-hook (intern (format "%s-hook" (car mode)))
               (lambda ()
                 (setq-local devdocs-current-docs (cdr mode)))))
   devdocs-major-mode-docs-alist)

  (setq devdocs-data-dir (expand-file-name "devdocs" user-emacs-directory))

  (defun devdocs-dwim()
    "Look up a DevDocs documentation entry.
Install the doc if it's not installed."
    (interactive)
    ;; Install the doc if it's not installed
    (mapc
     (lambda (slug)
       (unless (member slug (let ((default-directory devdocs-data-dir))
                              (seq-filter #'file-directory-p
                                          (when (file-directory-p devdocs-data-dir)
                                            (directory-files "." nil "^[^.]")))))
         (mapc
          (lambda (doc)
            (when (string= (alist-get 'slug doc) slug)
              (devdocs-install doc)))
          (devdocs--available-docs))))
     (alist-get major-mode devdocs-major-mode-docs-alist))

    ;; Lookup the symbol at point
    (devdocs-lookup nil (thing-at-point 'symbol t))))

(use-package diff-hl
  :commands (diff-hl-mode diff-hl-dired-mode))

(use-package diff-hl-flydiff
  :straight nil
  :after diff-hl
  :commands diff-hl-flydiff-mode)

;;;_ , diff-mode

(use-package diff-mode
  :commands diff-mode
  :custom
  (diff-mode-hook '(diff-delete-empty-files diff-make-unified smerge-mode))
  :custom-face
  (diff-added ((((background dark)) (:foreground "#FFFF9B9BFFFF"))
               (t (:foreground "DarkGreen"))))
  (diff-changed ((((background dark)) (:foreground "Yellow"))
                 (t (:foreground "MediumBlue"))))
  (diff-context ((((background dark)) (:foreground "White"))
                 (t (:foreground "Black"))))
  (diff-file-header ((((background dark)) (:foreground "Cyan" :background "Black"))
                     (t (:foreground "Red" :background "White"))))
  (diff-header ((((background dark)) (:foreground "Cyan"))
                (t (:foreground "Red"))))
  (diff-index ((((background dark)) (:foreground "Magenta"))
               (t (:foreground "Green"))))
  (diff-nonexistent ((((background dark)) (:foreground "#FFFFFFFF7474"))
                     (t (:foreground "DarkBlue")))))

(use-package diffview
  :commands (diffview-current diffview-region diffview-message))

;;;_ , dired

(use-package dired
  :straight nil
  :diminish dired-omit-mode
  :bind ("C-c j" . dired-two-pane)
  :bind (:map dired-mode-map
              ("j"     . dired)
              ("l"     . dired-up-directory)
              ("z"     . pop-window-configuration)
              ("e"     . ora-ediff-files)
              ("^"     . dired-up-directory)
              ("q"     . pop-window-configuration)
              ("Y"     . ora-dired-rsync)
              ("M-!"   . #'async-shell-command)
              ("<tab>" . dired-next-window)
              (";"     . dired-do-repeat-shell-command)
              ("M-G")
              ("M-s f"))
  :hook (dired-mode . dired-hide-details-mode)
  :custom
  (dired-clean-up-buffers-too nil)
  (dired-dwim-target t)
  (dired-hide-details-hide-information-lines nil)
  (dired-hide-details-hide-symlink-targets nil)
  ;; jww (2023-05-13): This does not work on all Tramp hosts
  ;; (dired-listing-switches "--group-directories-first -lah")
  (dired-listing-switches "-lah")
  (dired-no-confirm
   '(byte-compile chgrp chmod chown copy hardlink symlink touch))
  (dired-omit-mode nil t)
  ;; (dired-omit-size-limit 60000)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-shell-command-history '("mpv --fs --volume=50 --osd-bar-align-y=1 *"))
  :functions (dired-dwim-target-directory)
  :preface
  (defun dired-two-pane ()
    (interactive)
    (push-window-configuration)
    (let ((here default-directory))
      (delete-other-windows)
      (dired "~/dl")
      (split-window-horizontally)
      (dired here)))

  (defun dired-next-window ()
    (interactive)
    (let ((next (car (cl-remove-if-not
                      #'(lambda (wind)
                          (with-current-buffer (window-buffer wind)
                            (eq major-mode 'dired-mode)))
                      (cdr (window-list))))))
      (when next
        (select-window next))))

  (defvar mark-files-cache (make-hash-table :test #'equal))

  (defun mark-similar-versions (name)
    (let ((pat name))
      (if (string-match "^\\(.+?\\)-[0-9._-]+$" pat)
          (setq pat (match-string 1 pat)))
      (or (gethash pat mark-files-cache)
          (ignore (puthash pat t mark-files-cache)))))

  (defun dired-mark-similar-version ()
    (interactive)
    (setq mark-files-cache (make-hash-table :test #'equal))
    (dired-mark-sexp '(mark-similar-versions name)))

  (defun dired-do-repeat-shell-command (&optional arg file-list)
    (interactive
     (let ((files (dired-get-marked-files t current-prefix-arg)))
       (list
        current-prefix-arg
        files)))
    (let ((display-buffer-alist
           (list
            (cons
             "\\*Async Shell Command\\*.*"
             (cons #'display-buffer-no-window nil)))))
      (dired-do-async-shell-command
       (car dired-shell-command-history) arg file-list)))

  (defun ora-dired-rsync (dest)
    (interactive
     (list
      (expand-file-name
       (read-file-name "Rsync to: " (dired-dwim-target-directory)))))
    (let ((files (dired-get-marked-files
                  nil current-prefix-arg))
          (tmtxt/rsync-command "rsync -aP "))
      (dolist (file files)
        (setq tmtxt/rsync-command
              (concat tmtxt/rsync-command
                      (shell-quote-argument file)
                      " ")))
      (setq tmtxt/rsync-command
            (concat tmtxt/rsync-command
                    (shell-quote-argument dest)))
      (async-shell-command tmtxt/rsync-command "*rsync*")
      (other-window 1)))

  (defun ora-ediff-files ()
    (interactive)
    (let ((files (dired-get-marked-files))
          (wnd (current-window-configuration)))
      (if (<= (length files) 2)
          (let ((file1 (car files))
                (file2 (if (cdr files)
                           (cadr files)
                         (read-file-name
                          "file: "
                          (dired-dwim-target-directory)))))
            (if (file-newer-than-file-p file1 file2)
                (ediff-files file2 file1)
              (ediff-files file1 file2))
            (add-hook 'ediff-after-quit-hook-internal
                      `(lambda ()
                         (setq ediff-after-quit-hook-internal nil)
                         (set-window-configuration ,wnd))))
        (error "No more than 2 files should be marked")))))

(use-package dired-follow
  :straight nil
  :no-require t
  :after dired
  :bind (:map dired-mode-map
              ("F" . dired-follow-mode))
  :preface
  (defun do-dired-display-file (_)
    (dired-display-file))

  (define-minor-mode dired-follow-mode
    "Diplay file at point in dired after a move."
    :lighter " dired-f"
    :global t
    :group 'dired
    (if dired-follow-mode
        (advice-add 'dired-next-line :after #'do-dired-display-file)
      (advice-remove 'dired-next-line #'do-dired-display-file))))

(use-package dired-hist
  :after dired
  :straight (dired-hist :type git :host github :repo "karthink/dired-hist")
  :bind (:map  dired-mode-map
               ("l" . dired-hist-go-back)
               ("r" . dired-hist-go-forward))
  :config
  (dired-hist-mode +1))

(use-package dired-narrow
  :after dired
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

(use-package dired-git
  :hook (dired-mode-hook . #'dired-git-mode))

(use-package dired-imenu
  :after dired)

(use-package dired-rsync
  :config
  (defun my/dired-rsync-update-modeline (&optional err ind)
    (let ((job-count (length (dired-rsync--get-active-buffers))))
      (cond
       ;; error has occurred
       (err (alert (format "%d job failed: %s" job-count err)
                   :severity 'urgent
                   :title "dired-rsync"))
       ((zerop job-count) (alert "done"
                                 :severity 'normal
                                 :title "dired-rsync")))))

  (advice-add #'dired-rsync--update-modeline
              :after #'my/dired-rsync-update-modeline))

(use-package dired-toggle
  :bind ("C-c ~" . dired-toggle)
  :preface
  (defun my-dired-toggle-mode-hook ()
    (interactive)
    (visual-line-mode 1)
    (setq-local visual-line-fringe-indicators '(nil right-curly-arrow))
    (setq-local word-wrap nil))
  :hook (dired-toggle-mode . my-dired-toggle-mode-hook))

;;;_ , disable-mouse-mode

(use-package disable-mouse
  :diminish disable-mouse-global-mode
  :config
  (global-disable-mouse-mode +1))

;;;_ , display-line-numbers-

(use-package display-line-numbers
  :hook (org-mode prog-mode) . #'display-line-numbers-mode)

;;;_ , docker

(use-package docker
  :defer
  ;; BULK-ENSURE :ensure t
  :diminish)

(use-package docker-compose-mode
  :mode "docker-compose.*\.yml\\'")

;;;_ , dockerfile

(use-package dockerfile-mode
  ;; BULK-ENSURE :ensure t
  :mode "Dockerfile[a-zA-Z.-]*\\'")

(use-package doom-modeline
  :preface
  (defvar lui-tracking-propertized '())
  (defvar lui-tracking-shortened '())
  (defvar lui-tracking-modified '())

  (defun inc-tracking ()
    "Increment reference count for `current-buffer’."
    (if-let ((p (assoc (current-buffer) lui-tracking-modified)))
        (setcdr p (+ 1 (cdr p)))
      (push (cons (current-buffer) 1) lui-tracking-modified)
      (let ((te lui-tracking-modified)
            (ss "")
            (pr ""))
        (while (> (length te) 0)
          (cond
           ((string-match "*Slack - \\|slack" te)
            (setq pr (concat pr (all-the-icons-faicon "slack")))
            (setq te (replace-match "" nil nil te)))

           ((string-match "clojurians - \\|clojure" te)
            (setq pr (concat pr (all-the-icons-alltheicon "clojure-line")))
            (setq te (replace-match "" nil nil te)))

           (t
            (setq pr (concat pr te))
            (setq ss (concat ss te))
            (setq te ""))))
        (push (cons s ss) lui-tracking-shortened)
        (push (cons s pr) lui-tracking-propertized))
      (setq lui-tracking-shortened
            (let ((the-dict
                   (shorten-strings
                    (mapcar
                     (lambda (x)
                       (replace-regexp-in-string
                        (rx (* (any " \t\n"))) "" (buffer-name (cdr x))))
                     lui-tracking-shortened))))
              (mapcar (lambda (y)
                        (cons (car y) (assq (cdr y) the-dict)))
                      lui-tracking-shortened)))
      1))

  (defun zero-tracking ()
    "Zero reference count for `current-buffer’."
    (map (lambda (x)
           (setq x
                 (remove* (current-buffer)
                          x :test 'eq :key 'car)))
         (list lui-tracking-modified
               lui-tracking-shortened
               lui-tracking-propertized)))

  (defun lui-propertized (s)
    "Given string, return smallest string and propertized forms.")


  :hook (after-init . (lambda () (doom-modeline-mode 1)))
  :custom
  (doom-modeline-irc nil)
  (doom-modeline-project-detection 'auto)
  (doom-modeline-buffer-file-name-style 'truncate-except-project)
  :config
  (doom-modeline-def-segment ati-erc-server-alive
    "An `all-the-icons’ segment for erc-server-process-alive"
    (if (erc-server-process-alive)
        (all-the-icons-faicon "bolt")
      (all-the-icons-faicon "close")))

  (doom-modeline-def-segment ati-lui-track
    "An `all-the-icons' segment for tracking-mode"
    (when (bound-and-true-p erc-track-mode)
      (let ((shortened (mapcar*
                        #'cons (mapcar
                                (lambda (x) (buffer-name (car x)))
                                erc-modified-channels-alist)
                        (erc-unique-substrings
                         (mapcar
                          (lambda (x)
                            (let ((name (buffer-name (car x))))
                              (cond
                               ((string-match "#twitter_" name)
                                (substring name 9))
                               ((string-match "#" name)
                                (substring name 2))
                               (t name))))

                          erc-modified-channels-alist)))))
        (mapcar (lambda (b)
                  (propertize
                   (let ((name (buffer-name (car b)))
                         (the-count (number-to-string (cadr b))))
                     (cond
                      ((string-match "#mercurial" name)
                       (concat
                        " "
                        (all-the-icons-faicon "mercury" :v-adjust 0.1)
                        the-count))


                      ((string-match "#bitbucket" name)
                       (concat
                        " "
                        (all-the-icons-faicon "bitbucket" :v-adjust 0.1)
                        the-count))

                      ((string-match "#twitter_" name)
                       (concat " "
                               (all-the-icons-faicon "twitter" :v-adjust 0.1)
                               (let ((short-lookup (assoc name shortened)))
                                 (if short-lookup (cdr short-lookup) name))
                               ":"
                               the-count))

                      (t (concat " "
                                 (all-the-icons-material
                                  "person" :v-adjust -0.1)
                                 name
                                 ":"
                                 the-count))))
                   'face '(:height 0.9 :inherit)
                   'help-echo "ERC"))
                erc-modified-channels-alist))))

  (doom-modeline-def-segment ati-erc-track
    "An `all-the-icons' segment for `erc-track’"
    (when (bound-and-true-p erc-track-mode)
      (let ((shortened (mapcar* #'cons
                                (mapcar
                                 (lambda (x) (buffer-name (car x)))
                                 erc-modified-channels-alist)
                                (erc-unique-substrings
                                 (mapcar
                                  (lambda (x)
                                    (let ((name (buffer-name (car x))))
                                      (cond
                                       ((string-match "#twitter_" name)
                                        (substring name 9))
                                       ((string-match "#" name)
                                        (substring name 2))
                                       (t name))))

                                  erc-modified-channels-alist)))))
        (mapcar (lambda (b)
                  (propertize
                   (let ((name (buffer-name (car b)))
                         (the-count (number-to-string (cadr b))))
                     (cond
                      ((string-match "#mercurial" name)
                       (concat
                        " "
                        (all-the-icons-faicon "mercury" :v-adjust 0.1)
                        the-count))


                      ((string-match "#bitbucket" name)
                       (concat
                        " "
                        (all-the-icons-faicon "bitbucket" :v-adjust 0.1)
                        the-count))

                      ((string-match "#twitter_" name)
                       (concat " "
                               (all-the-icons-faicon "twitter" :v-adjust 0.1)
                               (let ((short-lookup (assoc name shortened)))
                                 (if short-lookup (cdr short-lookup) name))
                               ":"
                               the-count))

                      (t (concat " "
                                 (all-the-icons-material
                                  "person" :v-adjust -0.1)
                                 name
                                 ":"
                                 the-count))))
                   'face '(:height 0.9 :inherit)
                   'help-echo "ERC"))
                erc-modified-channels-alist))))

  (add-to-list 'global-mode-string '(:eval
                                     (doom-modeline-segment--ati-erc-track))
               'APPEND))

;;;_ , dumb-jump

(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g b" . dumb-jump-back)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
  (defhydra dumb-jump-hydra (:color blue :columns 3)
    "Dumb Jump"
    ("j" dumb-jump-go "Go")
    ("o" dumb-jump-go-other-window "Other window")
    ("e" dumb-jump-go-prefer-external "Go external")
    ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
    ("i" dumb-jump-go-prompt "Prompt")
    ("l" dumb-jump-quick-look "Quick look")
    ("b" dumb-jump-back "Back")))

(use-package dwim-shell-command)

;;;_ , dynamic-spaces

(use-package dynamic-spaces
  :config
  (dynamic-spaces-global-mode 1))

(use-package easy-kill
  :unless noninteractive
  :config
  (global-set-key [remap kill-ring-save] #'easy-kill)
  (global-set-key [remap mark-sexp] #'easy-mark))

(use-package easy-kill-extras
  :disabled t ;; various dependencies on external packages
  ;; BULK-ENSURE :ensure t
  :after easy-kill
  :bind (("M-@" . 'easy-mark-word)
         ("C-M-@" . 'easy-mark-sexp)
         ([remap zap-to-char] . 'easy-mark-to-char))

  :bind (:map easy-kill-base-map
              ("o" . 'easy-kill-er-expand)
              ("i" . 'easy-kill-er-unexpand))

  :config
  (add-to-list 'easy-kill-alist '(?^ backward-line-edge ""))
  (add-to-list 'easy-kill-alist '(?$ forward-line-edge ""))
  (add-to-list 'easy-kill-alist '(?b buffer ""))
  (add-to-list 'easy-kill-alist '(?< buffer-before-point ""))
  (add-to-list 'easy-kill-alist '(?> buffer-after-point ""))
  (add-to-list 'easy-kill-alist '(?f string-to-char-forward ""))
  (add-to-list 'easy-kill-alist '(?F string-up-to-char-forward ""))
  (add-to-list 'easy-kill-alist '(?t string-to-char-backward ""))
  (add-to-list 'easy-kill-alist '(?T string-up-to-char-backward "")))

;;;_ , ediff

(use-package ediff
  :bind (("C-c = b" . ediff-buffers)
         ("C-c = B" . ediff-buffers3)
         ("C-c = c" . compare-windows)
         ("C-c = =" . ediff-files)
         ("C-c = f" . ediff-files)
         ("C-c = F" . ediff-files3)
         ("C-c = m" . count-matches)
         ("C-c = r" . ediff-revision)
         ("C-c = p" . ediff-patch-file)
         ("C-c = P" . ediff-patch-buffer)
         ("C-c = l" . ediff-regions-linewise)
         ("C-c = w" . ediff-regions-wordwise))
  :custom
  (ediff-combination-pattern
   '("<<<<<<< A: HEAD" A "||||||| Ancestor" Ancestor "=======" B ">>>>>>> B: Incoming"))
  (ediff-diff-options "-w")
  (ediff-highlight-all-diffs nil)
  (ediff-show-clashes-only t)
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  :custom-face
  (ediff-current-diff-C ((t (:extend t :background "#222200"))))
  :init
  (defun test-compare ()
    (interactive)
    (delete-other-windows)
    (let ((here (point)))
      (search-forward "got:")
      (split-window-below)
      (goto-char here))
    (search-forward "expected:")
    (call-interactively #'compare-windows))

  (defun test-ediff ()
    (interactive)
    (goto-char (point-min))
    (search-forward "expected:")
    (forward-line 1)
    (goto-char (line-beginning-position))
    (let ((begin (point)))
      (search-forward "(")
      (goto-char (match-beginning 0))
      (forward-sexp)
      (let ((text (buffer-substring begin (point)))
            (expected (get-buffer-create "*expected*")))
        (with-current-buffer expected
          (erase-buffer)
          (insert text))
        (search-forward "got:")
        (forward-line 1)
        (goto-char (line-beginning-position))
        (setq begin (point))
        (search-forward "(")
        (goto-char (match-beginning 0))
        (forward-sexp)
        (setq text (buffer-substring begin (point)))
        (let ((got (get-buffer-create "*got*")))
          (with-current-buffer got
            (erase-buffer)
            (insert text))
          (ediff-buffers expected got))))))

;;;_ , edit-indirect

(use-package edit-indirect
  :bind ("C-c ’" . indirect-region))

;;;_ , edit-server

;; (use-package edit-server
;;   :init
;;   (progn
;;     (add-hook 'after-init-hook 'server-start t)
;;     (add-hook 'after-init-hook 'edit-server-start t)))

(use-package lsp-pyright
  :ensure t)

(use-package eglot
  :commands eglot
  :custom
  (eglot-autoshutdown t)
  :config
  (if (executable-find "pip")
      (unless (executable-find "pyright-python-langserver")
        (shell-command "pip install pyright"))
    (unless (executable-find "jedi-language-server")
      (shell-command "pip install jedi-language-server"))
    (unless (executable-find "pylsp")
      (shell-command "pip install pylsp"))
    )

  (setq read-process-output-max (* 1024 1024))
  (add-to-list 'eglot-server-programs
               `(python-mode
                 . ,(eglot-alternatives
                     '("pylsp"
                       "jedi-language-server"
                       ("pyright-langserver" "--stdio")))))
  (add-hook 'eglot-managed-mode-hook
            #'(lambda ()
                ;; Show flymake diagnostics first.
                (setq eldoc-documentation-functions
                      (cons #'flymake-eldoc-function
                            (remove #'flymake-eldoc-function
                                    eldoc-documentation-functions))))))

(use-package eglot-orderless
  :straight nil
  :no-require t
  :after (eglot orderless)
  :config
  (add-to-list 'completion-category-overrides
               '(eglot (styles orderless basic))))

;;;_ , elfeed

(use-package elfeed
  :unless noninteractive
  :after elfeed-org
  :commands (elfeed)
  :init

  (defvar feed-patterns
    '((feedburner "http://feeds.feedburner.com/%s")
      (gmane     "http://rss.gmane.org/topics/complete/gmane.%s")
      (subreddit "http://www.reddit.com/r/%s/.rss"))
    "how certain types of feeds automatically expand.")

  :custom
  (elfeed-db-directory
   (ensure-directory (locate-user-emacs-file "elfeed/db")))
  (elfeed-search-filter "@6-months-ago")
;;;;  :hook (elfeed-search-mode . #'set-scroll-margin)
  :config

  (defun elfeed-v-mpv (url)
    "Watch a video from URL in MPV"
    (async-shell-command (format "mpv %s" url)))

  (defun elfeed-view-mpv (&optional use-generic-p)
    "Youtube-feed link"
    (interactive "P")
    (let ((entries (elfeed-search-selected)))
      (cl-loop for entry in entries
               do (elfeed-untag entry 'unread)
               when (elfeed-entry-link entry)
               do (elfeed-v-mpv it))
      (mapc #'elfeed-search-update-entry entries)
      (unless (use-region-p) (forward-line))))

  (define-key elfeed-search-mode-map (kbd "v") 'elfeed-view-mpv)

  (defvar ap/elfeed-update-complete-hook nil
    "Functions called with no arguments when `elfeed-update' is finished.")

  (defvar ap/elfeed-updates-in-progress 0
    "Number of feed updates in-progress.")

  (defvar ap/elfeed-search-update-filter nil
    "The filter when `elfeed-update' is called.")

  (defun ap/elfeed-update-complete-hook (&rest ignore)
    "When update queue is empty, run `ap/elfeed-update-complete-hook' functions."
    (when (= 0 ap/elfeed-updates-in-progress)
      (run-hooks 'ap/elfeed-update-complete-hook)))

  (add-hook 'elfeed-update-hooks #'ap/elfeed-update-complete-hook)

  (defun ap/elfeed-update-message-completed (&rest _ignore)
    (message "Feeds updated")
    (notifications-notify :title "Elfeed" :body "Feeds updated."))

  (add-hook 'ap/elfeed-update-complete-hook #'ap/elfeed-update-message-completed)

  (defun ap/elfeed-search-update-restore-filter (&rest ignore)
    "Restore filter after feeds update."
    (when ap/elfeed-search-update-filter
      (elfeed-search-set-filter ap/elfeed-search-update-filter)
      (setq ap/elfeed-search-update-filter nil)))

  (add-hook 'ap/elfeed-update-complete-hook #'ap/elfeed-search-update-restore-filter)

  (defun ap/elfeed-search-update-save-filter (&rest ignore)
    "Save and change the filter while updating."
    (setq ap/elfeed-search-update-filter elfeed-search-filter)
    (setq elfeed-search-filter "#0"))

  ;; NOTE: It would be better if this hook were run before starting the feed updates, but in
  ;; `elfeed-update', it happens afterward.
  (add-hook 'elfeed-update-init-hooks #'ap/elfeed-search-update-save-filter)

  (defun ap/elfeed-update-counter-inc (&rest ignore)
    (cl-incf ap/elfeed-updates-in-progress))

  (advice-add #'elfeed-update-feed :before #'ap/elfeed-update-counter-inc)

  (defun ap/elfeed-update-counter-dec (&rest ignore)
    (cl-decf ap/elfeed-updates-in-progress)
    (when (< ap/elfeed-updates-in-progress 0)
      ;; Just in case
      (setq ap/elfeed-updates-in-progress 0)))

  (add-hook 'elfeed-update-hooks #'ap/elfeed-update-counter-dec)

  (progn
    (if (fboundp 'set-scroll-margin)
        (set-scroll-margin))
    ;; this package does not use customize
    ;; elfeed-search-date-face
    ;; elfeed-search-title-face
    ;; elfeed-search-feed-face
    ;; elfeed-search-tag-face

    (add-hook 'elfeed-new-entry-hook
              (elfeed-make-tagger :before "4 weeks ago"
                                  :remove 'unread))

    ;; Mark all as read
    (defun elfeed-mark-all-as-read ()
      (interactive)
      (mark-whole-buffer)
      (elfeed-search-untag-all-unread))

    (bind-keys
     :map elfeed-search-mode-map
     ("R" . elfeed-mark-all-as-read))
    (run-with-idle-timer 3000 t 'elfeed-update)))

(use-package elfeed-goodies
  :config
  (elfeed-goodies/setup))

;;;_ , elfeed-org

(use-package elfeed-org
  :unless noninteractive
  :after org
  :config
  (setq rmh-elfeed-org-files
        (directory-files
         (ensure-user-dir "elfeed-org") t "\\.org$"))
  (elfeed-org))

(use-package elfeed-tube
  :straight (:host github :repo "karthink/elfeed-tube")
  :after elfeed
  :config
  ;; (setq elfeed-tube-auto-save-p nil) ;; t is auto-save (not default)
  ;; (setq elfeed-tube-auto-fetch-p t) ;;  t is auto-fetch (default)
  (elfeed-tube-setup)

  :bind (:map elfeed-show-mode-map
              ("F" . elfeed-tube-fetch)
              ([remap save-buffer] . elfeed-tube-save)
              :map elfeed-search-mode-map
              ("F" . elfeed-tube-fetch)
              ([remap save-buffer] . elfeed-tube-save)))

(use-package elisp-depend
  :commands elisp-depend-print-dependencies)

;;;_ , elisp-slime-nav-mode

(use-package elisp-slime-nav
  :unless noninteractive
  ;; BULK-ENSURE :ensure t
  :diminish elisp-slime-nav-mode
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook (lambda () (elisp-slime-nav-mode t)))))

;;;_ , elmacro
(use-package elmacro
  ;; BULK-ENSURE :ensure t
  :diminish elmacro-mode
  :config
  (elmacro-mode 1))

;;;_ , eloud
(use-package eloud
  :disabled t
  ;; BULK-ENSURE :ensure t
  :defer 1)

(use-package elpy
  :init
  (elpy-enable))

(use-package ement
  :straight (ement :host github :repo "alphapapa/ement.el"))

;;;_ , emojify

(use-package emojify
  :if (and (display-graphic-p)
           (not noninteractive))
  :hook
  (after-init . global-emojify-mode)
  ;; ((text-mode tabulated-list-mode)
  ;;  #'emojify-mode)
  )

;;;_ , erc

;; (defun start-irc (&optional arg)
;;   (interactive "P")
;;   (if arg
;;       (pcase-dolist (`(,server . ,nick)
;;                      '(("a.server.host"     . "yourusername")
;;                        ("another.host"      . "usernamethere")
;;                        ("yet.a.third.host"  . "third identity")
;;                        ))
;;         (erc-tls :server server :port 6697 :nick (concat nick "_")
;;                  :password (lookup-password server nick 6697)))
;;     (let ((pass (lookup-password "irc.freenode.net" "johnw" 6697)))
;;       (when (> (length pass) 32)
;;         (error "Failed to read ZNC password"))
;;       (erc :server "127.0.0.1" :port 6697 :nick "johnw"
;;            :password (concat "johnw/gitter:" pass))
;;       (sleep-for 5)
;;       (erc :server "127.0.0.1" :port 6697 :nick "johnw"
;;            :password (concat "johnw/plclub:" pass))
;;       (sleep-for 5)
;;       (erc :server "127.0.0.1" :port 6697 :nick "johnw"
;;            :password (concat "johnw/freenode:" pass)))))

;;   (defun start-irc ()
;;     (interactive "P")
;;     (erc-tls :server server :port 6697 :nick (concat nick "_")
;;              :password (lookup-password server nick 6697)))
;; )

(use-package engine-mode ;; Bound to C-x / <key>
  :defer 3
  :config
  (defengine amazon
    "http://www.amazon.com/s/ref=nb_sb_noss?url=search-alias=aps&field-keywords=%s"
    :keybinding "a")

  (defengine duckduckgo
    "https://duckduckgo.com/?q=%s"
    :keybinding "d")

  (defengine github
    "https://github.com/search?ref=simplesearch&q=%s"
    :keybinding "g")

  (defengine google-images
    "http://www.google.com/images?hl=en&source=hp&biw=1440&bih=795&gbv=2&aq=f&aqi=&aql=&oq=&q=%s"
    :keybinding "i")

  (defengine google-maps
    "http://maps.google.com/maps?q=%s"
    :keybinding "m"
    :docstring "Mappin' it up.")

  (defengine stack-overflow
    "https://stackoverflow.com/search?q=%s"
    :keybinding "s")

  (defengine youtube
    "http://www.youtube.com/results?aq=f&oq=&search_query=%s"
    :keybinding "y")

  (defengine wikipedia
    "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
    :keybinding "w"
    :docstring "Searchin' the wikis.")
  (engine-mode t))

(use-package erc
  :straight nil
  :preface
  (require 'erc)
  ;; (require 'erc-sound)
  ;; (erc-sound-enable)
  :init
  (use-package erc-bitlbee-twitter-decorate
    :disabled t
    :load-path "/home/emacs/.emacs.d/lisp"
    :config (erc-bitlbee-twitter-decorate-mode 1))

  (ensure-user-dir "erc/logs")
  :config
  (erc-spelling-mode +1)

  (setq erc-header-line-format nil
        erc-input-line-position -1
        erc-timestamp-only-if-changed-flag t
        erc-timestamp-format "[%H:%M] "
        erc-datestamp-format " === [%Y-%m-%d %a] ===\n" ; mandatory ascii art
        erc-fill-prefix "      "
        erc-query-display 'buffer)

  (setq erc-fill-column 180
        erc-fill-function 'erc-fill-static
        erc-fill-static-center 16)

  (set-face-attribute 'erc-default-face nil
                      :font "Roboto Condensed" :width 'condensed)
  (add-hook 'erc-send-pre-hook #'define-abbrev-sedlike)

  ;; (bind-key "<tab>" 'completion-at-point erc-mode-map)
  ;; probably don’t want to bind tab anymore
  (bind-key "C-s" 'isearch-forward erc-mode-map)
  (bind-key "C-r" 'isearch-backward erc-mode-map)
  (erc-track-minor-mode 1)
  (erc-track-mode 1)
  (erc-truncate-mode +1)

  (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                  "324" "329" "332" "333" "353" "477")
        erc-track-showcount     +1)
  (setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))
  ;; in .emacs.d/lisp, from https://github.com/jweigley/lisp/dot-emacs

  (defun my-erc-bitlbee-query (nick)
    "Query someone from the &bitlbee channel."
    (interactive
     (list (completing-read
            "Nick: "
            (with-current-buffer "&bitlbee" erc-channel-users))))
    (with-current-buffer "&bitlbee" (erc-cmd-QUERY nick)))

  (defun switch-to-bitlbee ()
    (interactive)
    (switch-to-buffer-other-window "&bitlbee")
    (call-interactively 'erc-channel-names)
    (goto-char (point-max)))

  (bind-key "C-c b" 'switch-to-bitlbee erc-mode-map)

  (defun erc-cmd-CLEAR ()
    "Clears the current buffer"
    (erc-truncate-buffer-to-size 0))

  (defun erc-cmd-CLEARALL ()
    "Clears all ERC buffers"
    (setq erc-modified-channels-alist '())
    (mapc (lambda (buffer)
            (erc-truncate-buffer-to-size 0 (get-buffer buffer)))
          (erc-all-buffer-names)))

  (defun erc-receiverize-prompt ()
    (if (and (boundp 'erc-default-recipients)
             (erc-default-target))
        (erc-propertize (concat (erc-default-target) ">")
                        'read-only t 'rear-nonsticky t
                        'front-nonsticky t)
      (erc-propertize (concat "ERC>") 'read-only t
                      'rear-nonsticky t
                      'front-nonsticky t)))
  (defun erc-cmd-SHOW (&rest form)
    "Eval FORM and send the result and the original form as:
FORM => (eval FORM)."
    (let* ((form-string (mapconcat 'identity form " "))
           (result
            (condition-case err
                (eval (thing-at-point--read-from-whole-string form-string))
              (error
               (format "Error: %s" err)))))
      (erc-send-message (format "%s => %S" form-string result))))

  (defun erc-cmd-INFO (&rest ignore)
    "Send current info node."
    (unless (get-buffer "*info*")
      (error "No *info* buffer"))
    (let (output)
      (with-current-buffer "*info*"
        (let* ((file (file-name-nondirectory Info-current-file))
               (node Info-current-node))
          (setq output (format "(info \"(%s)%s\") <-- hit C-x C-e to evaluate"
                               file node))))
      (erc-send-message output)))

  (eval-when-compile
    (defvar erc-fools))

  (defun erc-cmd-FOOL (term &rest ignore)
    (add-to-list 'erc-fools term))

  (defun erc-cmd-UNFOOL (term &rest ignore)
    (setq erc-fools (delete term erc-fools)))

  (defun erc-cmd-OPME ()
    "Request chanserv to op me."
    (erc-message "PRIVMSG"
                 (format "chanserv op %s %s"
                         (erc-default-target)
                         (erc-current-nick)) nil))

  (defun erc-cmd-DEOPME ()
    "Deop myself from current channel."
    (erc-cmd-DEOP (format "%s" (erc-current-nick)))))

(use-package erc-bar
  :disabled t
  :after erc
  :load-path "/home/emacs/.emacs.d/lisp"
  :config (erc-bar-mode 1))

(use-package erc-sound-notify
  :disabled t
  :after erc
  :load-path "/home/emacs/.emacs.d/lisp"
  :config
  (add-to-list 'erc-sound-path
               (locate-user-emacs-file "imsounds/"))
  (maybe-load-user-file "erc-sound-notify-config.el")
  (setq erc-sound-notify-default-sound "pop.wav"
        erc-sound-notify-volume 0.01)
  (erc-sound-notify-mode 1))

(use-package erc-hl-nicks
  :after erc)

(use-package erc-yank
  ;; TBD no package, only exists on jweigley's gihub
  :disabled t
  :after erc
  :init
  (bind-key "C-y" 'erc-yank erc-mode-map))

;; (use-package erc
;;   :straight nil
;;   :preface
;;   (require 'erc)
;;   :custom-face
;;   (erc-default-face ((t (:font "Roboto Condensed" :width condensed))))
;;   :custom
;;   (erc-show-my-nick 1)
;;   (erc-interpret-mirc-color t)
;;   (erc-kill-buffer-on-part nil)
;;   (erc-kill-queries-on-quit nil)
;;   (erc-kill-server-buffer-on-quit t)
;;   (erc-query-display 'buffer)
;;   (erc-prompt (lambda () (concat "[" (buffer-name) "]")))
;;   (erc-auto-discard-away t)
;;   (erc-autoaway-idle-seconds 1200)
;;   (erc-server-coding-system '(utf-8 . utf-8))
;;   (erc-server-auto-reconnect t)
;;   (erc-server-reconnect-attempts t)
;;   :hook
;;   ;; update modules
;;   (erc-connect-pre . (lambda (x) (erc-update-modules))))

(use-package erc-timestamp
  :straight nil
  :no-require t
  :preface
  (make-variable-buffer-local
   (defvar erc-last-datestamp nil))
  (defun ks-timestamp (string)
    (erc-insert-timestamp-left string)
    (let ((datestamp (erc-format-timestamp
                      (current-time) erc-datestamp-format)))
      (unless (or (string= datestamp erc-last-datestamp)
                  (= (point-max) (point-min)))
        (erc-insert-timestamp-left datestamp)
        (setq erc-last-datestamp datestamp))))
  :custom
  (erc-insert-timestamp-function #'ks-timestamp)
  (erc-timestamp-only-if-changed-flag t)
  (erc-timestamp-format "[%H:%M] ")
  (erc-datestamp-format " === [%Y-%m-%d %a] ===\n") ; mandatory ascii art
  )

;; (use-package erc-fill
;;   :straight nil
;;   :preface
;;   ;; set erc-fill-column based on buffer size
;;   (make-variable-buffer-local 'erc-fill-column)
;;   (defun my/erc-fill-by-window-width ()
;;     "Adjust fill for every ERC window to match its width."
;;     (walk-windows
;;      (lambda (w)
;;        (let ((buffer (window-buffer w)))
;;          (when buffer
;;            (set-buffer buffer)
;;            (when (eq major-mode 'erc-mode)
;;              (setq erc-fill-column (- (window-width w) 2))))))))
;;   :custom
;;   (erc-fill-function 'erc-fill-static)
;;   (erc-fill-static-center 24)
;;   ;; :hook
;;   (window-configuration-change . #'my/erc-fill-by-window-width)
;;   )

;; (use-package erc-track
;;   :straight nil
;;   :custom
;;   (erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
;;                              "324" "329" "332" "333" "353" "477"))
;;   (erc-track-showcount +1)
;;   (erc-track-use-faces t)
;;   (erc-track-faces-priority-list '(erc-error-face
;;                                    erc-current-nick-face
;;                                    erc-keyword-face
;;                                    erc-nick-msg-face
;;                                    erc-direct-msg-face
;;                                    erc-dangerous-host-face
;;                                    erc-notice-face
;;                                    erc-prompt-face))
;;   (erc-track-priority-faces-only 'all)
;;   :config
;;   (erc-track-minor-mode +1)
;;   (erc-track-mode +1))

(use-package erc-log
  :straight nil
  :custom
  (erc-log-channels-directory (expand-file-name "erc/logs"
                                                user-emacs-directory))
  (erc-save-buffer-on-part t)
  :init
  (if (not (file-exists-p erc-log-channels-directory))
      (mkdir erc-log-channels-directory t))
  :config
  (erc-log-enable))

(use-package erc-view-log
  :config
  (add-to-list 'auto-mode-alist
               `(,(format "%s/.*\\.log"
                          (regexp-quote
                           (expand-file-name erc-log-channels-directory)))
                 . erc-view-log-mode)))

(use-package erc-spelling
  :straight nil
  :init (erc-spelling-mode t))

;; (use-package erc-autoaway
;;   :straight nil
;;   :config
;;   (add-to-list 'erc-modules 'autoaway))

(use-package erc-desktop-notifications
  :straight nil
  :config
  (add-to-list 'erc-modules 'notifications))

(use-package erc-image
  :config
  (add-to-list 'erc-modules 'image))

(use-package erc-tweet
  :config
  (add-to-list 'erc-modules 'tweet))

(use-package erc-youtube
  :config
  (add-to-list 'erc-modules 'youtube))

;; (use-package erc-colorize
;;   :disabled t
;;   :no-require t
;;   :straight nil
;;   :config
;;   (add-to-list 'erc-modules 'colorize))

;; (use-package erc-crypt
;;   :disabled t
;;   :straight nil)

;; (use-package erc-truncate
;;   :no-require t
;;   :straight nil
;;   ;; :functions erc-truncate-buffer-on-save
;;   :custom
;;   (erc-max-buffer-size 500000)
;;   (erc-truncate-buffer-on-save t)
;;   :init
;;   (erc-truncate-mode +1)
;;   :config
;;   (defun erc-cmd-CLEAR ()
;;     "Clears the current buffer"
;;     (erc-truncate-buffer-to-size 0))

;;   (defun erc-cmd-CLEARALL ()
;;     "Clears all ERC buffers"
;;     (setq erc-modified-channels-alist '())
;;     (mapc (lambda (buffer)
;;             (erc-truncate-buffer-to-size 0 (get-buffer buffer)))
;;           (erc-all-buffer-names)))

;;   :hook
;;   (erc-insert-post . #'erc-truncate-buffer))

(use-package erc-goodies
  :straight nil
  :config
  (add-to-list 'erc-modules 'smiley)
  (add-to-list 'erc-modules 'move-to-prompt)
  (add-to-list 'erc-modules 'keep-place)
  (add-to-list 'erc-modules 'irccontrols))

(use-package erc-speedbar
  :disabled t)

;; TODO: mark emacs frame as urgent
;; (add-hook 'erc-server-PRIVMSG-functions (lambda (proc parsed) (x-urgent) nil))
;; (add-hook 'erc-text-matched-hook (lambda (match-type nickuserhost msg) (x-urgent) nil))

;; urgency hint for Emacs frame
(defun x-urgency-hint (frame arg &optional source)
  "Set the x-urgency hint for the FRAME to ARG:
- If arg is nil, unset the urgency.
- If arg is any other value, set the urgency from SOURCE."
  (let* ((wm-hints (append (x-window-property
                            "WM_HINTS" frame "WM_HINTS"
                            source nil t) nil))
         (flags (car wm-hints)))
                                        ; (message flags)
    (setcar wm-hints
            (if arg
                (logior flags #x00000100)
              (logand flags #x1ffffeff)))
    (x-change-window-property "WM_HINTS" wm-hints frame "WM_HINTS" 32 t)))

(defun x-urgent (&optional arg)
  "Mark the current Emacs frame as requiring urgent attention.
With a prefix argument ARG which does not equal a boolean value
of nil, remove the urgency flag (which might or might not change
display, depending on the window manager)."
  (interactive "P")
  (let (frame (car (car (cdr (current-frame-configuration)))))
    (x-urgency-hint frame (not arg))))

;; (defun start-irc ()
;;   "Connect to IRC."
;;   (interactive)
;;   (load "~/.emacs.d/.erc-auth")
;;   (when (y-or-n-p "Do you want to start IRC? ")
;;     (start-irc-with-auth))
;;   (sr-speedbar-open)
;;   (erc-speedbar-browser))

;; (defun filter-server-buffers ()
;;   (delq nil
;;         (mapcar
;;          (lambda (x) (and (erc-server-buffer-p x) x))
;;          (buffer-list))))

;; (defun stop-irc ()
;;   "Disconnects from all irc servers."
;;   (interactive)
;;   (dolist (buffer (filter-server-buffers))
;;     (message "Server buffer: %s" (buffer-name buffer))
;;     (with-current-buffer buffer
;;       (erc-quit-server "Asta la vista"))))

;;;_ , eshell

(use-package eshell
  :commands (eshell eshell-command)
  :custom
  (eshell-directory-change-hook '(my-direnv-maybe-update))
  (eshell-directory-name (emacs-path "eshell"))
  (eshell-hist-ignoredups t)
  (eshell-history-size 50000)
  (eshell-ls-dired-initial-args '("-h"))
  (eshell-ls-exclude-regexp "~\\'")
  (eshell-ls-initial-args "-h")
  (eshell-modules-list
   '(eshell-alias
     eshell-basic
     eshell-cmpl
     eshell-dirs
     eshell-glob
     eshell-hist
     eshell-ls
     eshell-pred
     eshell-prompt
     eshell-rebind
     eshell-script
     ;; eshell-smart
     eshell-term
     eshell-unix
     eshell-xtra))
  (eshell-prompt-function
   (lambda nil
     (concat (abbreviate-file-name (eshell/pwd))
             (if (= (user-uid) 0)
                 " # " " $ "))))
  (eshell-rebind-keys-alist
   '(([(control 97)]
      . eshell-bol)
     ([home]
      . eshell-bol)
     ([(control 100)]
      . eshell-delchar-or-maybe-eof)
     ([backspace]
      . eshell-delete-backward-char)
     ([delete]
      . eshell-delete-backward-char)))
  (eshell-save-history-on-exit t)
  (eshell-stringify-t nil)
  (eshell-term-name "ansi")
  (eshell-visual-commands '("vi" "top" "screen" "less" "lynx" "rlogin" "telnet"))
  :preface
  (defvar eshell-isearch-map
    (let ((map (copy-keymap isearch-mode-map)))
      (define-key map [(control ?m)] 'eshell-isearch-return)
      (define-key map [return]       'eshell-isearch-return)
      (define-key map [(control ?r)] 'eshell-isearch-repeat-backward)
      (define-key map [(control ?s)] 'eshell-isearch-repeat-forward)
      (define-key map [(control ?g)] 'eshell-isearch-abort)
      (define-key map [backspace]    'eshell-isearch-delete-char)
      (define-key map [delete]       'eshell-isearch-delete-char)
      map)
    "Keymap used in isearch in Eshell.")

  (defun eshell-spawn-external-command (beg end)
    "Parse and expand any history references in current input."
    (save-excursion
      (goto-char end)
      (when (looking-back "&!" beg)
        (delete-region (match-beginning 0) (match-end 0))
        (goto-char beg)
        (insert "spawn "))))

  (defun eshell-initialize ()
    (defun eshell-spawn-external-command (beg end)
      "Parse and expand any history references in current input."
      (save-excursion
        (goto-char end)
        (when (looking-back "&!" beg)
          (delete-region (match-beginning 0) (match-end 0))
          (goto-char beg)
          (insert "spawn "))))

    (add-hook 'eshell-expand-input-functions 'eshell-spawn-external-command)

    (use-package em-unix
      :config
      (unintern 'eshell/su nil)
      (unintern 'eshell/sudo nil))
    :custom
    (eshell-cmpl-cycle-completions nil))

  :init
  (add-hook 'eshell-first-time-mode-hook 'eshell-initialize))

(use-package esh-buf-stack
  :after eshell
  :config
  (setup-eshell-buf-stack)
  (add-hook 'eshell-prepare-command-hook
            #'(lambda ()
                (bind-keys :map eshell-command-map
                           ("M-p" . eshell-push-command)))))

(use-package eshell-bookmark
  :hook (eshell-mode . eshell-bookmark-setup))

(use-package eshell-up
  :commands eshell-up)

(use-package eshell-z
  :after eshell)

(use-package esh-help
  :after eshell
  :config
  (setup-esh-help-eldoc))

(use-package eshell-toggle
  :after eshell
  :custom
  (eshell-toggle-size-fraction 3)
  (eshell-toggle-use-projectile-root t)
  (eshell-toggle-run-command nil)
  (eshell-toggle-init-function #'eshell-toggle-init-ansi-term)
  :bind
  ("C-x C-z" . eshell-toggle))

(use-package eshell-syntax-highlighting
  :after esh-mode
  :config
  ;; Enable in all Eshell buffers.
  (eshell-syntax-highlighting-global-mode +1))

;;;_ , ess

(use-package ess
  :config
  (setq ess-default-style 'RRR+))

(use-package ess-smart-equals
  :init
  (setq ess-smart-equals-extra-ops '(brace paren percent))
  :after (:any ess-r-mode inferior-ess-r-mode ess-r-transcript-mode)
  :config
  (ess-smart-equals-activate))

(use-package ess-smart-underscore
  :init
  :after (:any ess-r-mode inferior-ess-r-mode ess-r-transcript-mode))

(use-package etags
  :straight nil
  :bind ("M-T" . tags-search)
  :custom
  (tags-add-tables t)
  (tags-apropos-verbose t)
  (tags-case-fold-search nil)
  (tags-revert-without-query t))

;;;_ , eval-expr

(use-package eval-expr
  ;; BULK-ENSURE :ensure t
  :bind ("M-:" . eval-expr)
  :config
  (progn
    (setq eval-expr-print-function 'pp
          eval-expr-print-level 20
          eval-expr-print-length 100)

    (defun eval-expr-minibuffer-setup ()
      (set-syntax-table emacs-lisp-mode-syntax-table)
      (paredit-mode)
      (local-set-key (kbd "<tab>") #'my-elisp-indent-or-complete))))

;;;_ , eww

(use-package eww
  :config
  (use-package eww-lnum
    :defer t
    ;; BULK-ENSURE :ensure t
    :config
    (bind-key "f" #'eww-lnum-follow eww-mode-map)
    (bind-key "F" #'eww-lnum-universal eww-mode-map)))

;;;_ , expand-region

(use-package expand-region
  ;; BULK-ENSURE :ensure t
  :bind ("C-=" . er/expand-region))

(use-package change-inner
  :bind (("M-i"     . change-inner)
         ("M-o M-o" . change-outer)))

(use-package ffap
  :straight nil
  :bind (("C-c v"     . ffap)
         ("C-c <tab>" . ff-find-other-file)))

(use-package feather
  :config
  :hook (after-init . (lambda () (feather-mode +1))))

(use-package feed-discovery)

;;;_ , festival

(use-package festival
  :disabled t
  :load-path "/home/emacs/.emacs.d/lisp")

;;;_ , flycheck

(use-package flycheck
  :diminish "🐛"
  :after hydra
  :config
  (global-flycheck-mode +1)
  (when (require 'hydra nil 'noerror)
    (defhydra hydra-flycheck (:color blue)
      "
  ^
  ^Flycheck^          ^Errors^            ^Checker^
  ^────────^──────────^──────^────────────^───────^─────
  _q_ quit            _<_ previous        _?_ describe
  _M_ manual          _>_ next            _d_ disable
  _v_ verify setup    _f_ check           _m_ mode
  ^^                  _l_ list            _s_ select
  ^^                  ^^                  ^^
  "
      ("q" nil)
      ("<" flycheck-previous-error :color pink)
      (">" flycheck-next-error :color pink)
      ("?" flycheck-describe-checker)
      ("M" flycheck-manual)
      ("d" flycheck-disable-checker)
      ("f" flycheck-buffer)
      ("l" flycheck-list-errors)
      ("m" flycheck-mode)
      ("s" flycheck-select-checker)
      ("v" flycheck-verify-setup))))

(use-package flycheck-aspell
  :after flycheck
  :config
  (progn
    (add-to-list 'flycheck-checkers 'tex-aspell-dynamic)
    (setq ispell-program-name "aspell"
          ispell-silently-savep t)
    (defun flycheck-maybe-recheck (_)
      (when (bound-and-true-p flycheck-mode)
        (flycheck-buffer)))
    (advice-add #'ispell-pdict-save :after #'flycheck-maybe-recheck)))

(use-package flycheck-bashate
  :after flycheck
  :defer t
  :config
  (progn
    (flycheck-bashate-setup)))

(use-package flycheck-cask
  :after flycheck
  :defer t
  :config
  (progn
    (flycheck-cask-setup)))

(use-package flycheck-clj-kondo
  :after (flycheck (:any clojurescript-mode clojure-mode)))

(use-package flycheck-clojure
  :after flycheck
  :config
  (progn
    (flycheck-clojure-setup)))

(use-package flycheck-color-mode-line
  :disabled t
  :after flycheck
  :defer t
  :hook (flycheck-mode . flycheck-color-mode-line-mode))

(use-package flycheck-haskell
  :after flycheck
  :defer t
  :config
  (progn
    (flycheck-haskell-setup)))

(use-package flycheck-joker
  :disabled t
  :after flycheck)

(use-package flycheck-kotlin
  :after flycheck
  :defer t
  :config
  (progn
    (flycheck-kotlin-setup)))

(use-package flycheck-package
  :after flycheck
  :defer t
  :config
  (flycheck-package-setup))

(use-package flycheck-perl6
  :disabled t
  :after flycheck
  :defer t
  :hook (prog-mode . flycheck-mode))

(use-package flycheck-plantuml
  :after flycheck
  :defer t
  :config
  (progn
    (flycheck-plantuml-setup)))

(use-package flycheck-inline
  :after flycheck
  :defer t
  :config
  (flycheck-inline-enable))

(use-package flycheck-pos-tip
  :after flycheck
  :defer t
  :config
  (flycheck-pos-tip-mode))

(use-package flycheck-pyflakes
  :after flycheck
  :defer t)

(use-package flycheck-rtags
  :after flycheck
  :defer t
  :config)

(use-package flycheck-yamllint
  :after flycheck
  :defer t
  :config
  (progn
    (flycheck-yamllint-setup)))

(use-package avy-flycheck
  :disabled t
  :after flycheck
  :bind ("C-\"" . avy-flycheck-goto-error))

(use-package flycheck-posframe
  :disabled t
  :after flycheck
  :config (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))

;;;_ , flymake

(use-package flymake
  :diminish "🔦"
  :init
  (add-hook 'find-file-hook 'flymake-find-file-hook))

(use-package flymake-grammarly
  :after flymake
  :hook ((text-mode-hook
          latex-mode-hook
          org-mode-hook
          markdown-mode-hook) . #'flymake-kondor-setup))

(use-package flymake-kondor
  :after flymake
  :hook (clojure-mode . flymake-kondor-setup))

;;;_ , flymake-shellcheck

(use-package flymake-shellcheck
  :if (executable-find "shellcheck")
  :after flymake
  :defer t
  :hook (sh-mode-hook . #'flymake-shellcheck-load))

;;;_ , flyspell

(use-package flyspell
  :diminish "📚"
  ;; :bind (("C-c i b" . flyspell-buffer)
  ;;        ("C-c i f" . flyspell-mode))
  :commands flyspell-mode
  :init (progn
          ;; (add-hook 'prog-mode-hook #'flyspell-prog-mode)
          (dolist (mode-hook '(text-mode-hook org-mode-hook LaTeX-mode-hook))
            (add-hook mode-hook #'flyspell-mode)))
  ;; :hook (prog-mode . 'flyspell-prog-mode)
  ;; :hook (text-mode . 'flyspell-mode)
  :custom
  (flyspell-abbrev-p t)
  (flyspell-use-global-abbrev-table-p t)
  :config
  (unbind-key "C-." flyspell-mode-map)
  (add-hook 'prog-mode-hook (lambda ()
                              (flyspell-prog-mode)))
  (defun my-flyspell-maybe-correct-transposition (beg end candidates)
    (unless (let (case-fold-search)
              (string-match "\\`[A-Z0-9]+\\'"
                            (buffer-substring-no-properties beg end)))
      (flyspell-maybe-correct-transposition beg end candidates))))

(use-package flyspell-correct-ivy
  :disabled t
  :after flyspell
  :bind ("C-;" . 'flyspell-correct-previous-word-generic))

(use-package flyspell-correct-popup
  :after flyspell
  :bind ("C-;" . 'flyspell-correct-previous-word-generic))

(use-package focus
  :commands focus-mode)

;;;_ , fold-dwim

(use-package fold-dwim
  :defer t
  :bind (("<f13>" . fold-dwim-toggle)
         ("<f14>" . fold-dwim-hide-all)
         ("<f15>" . fold-dwim-show-all)))

(use-package font-lock
  :straight nil
  :defer t
  :custom
  (global-font-lock-mode t)
  (font-lock-support-mode 'jit-lock-mode)
  (font-lock-verbose nil))

(use-package font-lock-studio
  :commands (font-lock-studio
             font-lock-studio-region))

(use-package free-keys
  :commands free-keys)

(use-package gcmh
  :delight gcmh-mode
  :init
  (gcmh-mode +1))

(use-package ghub
  :defer t
  :config
  (require 'auth-source-pass)
  (defvar my-ghub-token-cache nil)
  (advice-add
   'ghub--token :around
   #'(lambda (orig-func host username package &optional nocreate forge)
       (or my-ghub-token-cache
           (setq my-ghub-token-cache
                 (funcall orig-func host username package nocreate forge))))))

(use-package forge
  :after magit)

;;;_ , ggtags

(use-package ggtags
  :disabled t
  :config
  (setq gtags-path-style 'relative)
  :bind ("C-x \\" . ggtags-find-tag-dwim))

;;;_ , gist

(use-package gist
  :defer t
  :bind ("C-c G" . gist-region-or-buffer))

;;;_ , git-auto-commit

(use-package git-auto-commit-mode
  :commands (gac-commit gac)
  :config
  (defun gac ()
    (interactive)
    (gac-commit)))

;;;_ , git-gutter

(use-package git-gutter
  :if (not (display-graphic-p))
  :diminish git-gutter-mode
  :hook ('prog-mode . 'git-gutter-mode))

;;;_ , git-gutter-fringe

(use-package git-gutter-fringe
  :if (display-graphic-p)
  :diminish git-gutter-mode
  :hook ('prog-mode . 'git-gutter-mode))

;;;_ ; git-timemachine

(use-package git-timemachine
  :unless noninteractive
  :commands git-timemachine)

;;;_ , gnus

;; (use-package dot-gnus
;;   :bind (("M-G"   . switch-to-gnus)
;;          ("C-x m" . compose-mail))
;; :init
;; (progn
;;   (setq gnus-init-file (locate-user-emacs-file "dot-gnus")
;;         gnus-home-directory "~/Messages/Gnus/"))

(use-package all-the-icons-gnus
  :disabled t
  :defer t
  :after dot-gnus
  :config
  (all-the-icons-gnus-setup))

;;;_ , go-mode

(use-package go-mode
  :defer t
  :config
  (progn
    (use-package flymake-go)

    (defun my-go-mode-hook ()
      (abbrev-mode 1)
      (gtags-mode 1)
      (whitespace-mode 1)
      (which-function-mode 1)
      (yas/minor-mode 1)

      (diminish 'gtags-mode)
      (diminish 'hs-minor-mode))

    (add-hook 'go-mode-hook 'my-go-mode-hook)))

(use-package go-mod-mode
  :disabled t
  :config
  (progn
    (flycheck-go-mod-setup)))

(use-package go-eldoc
  :unless noninteractive
  :after go-mode)

(use-package goggles
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse t))

;;;_ , google-this
(use-package google-this
  :diminish google-this-mode
  :config (google-this-mode))

(use-package goto-addr
  :unless noninteractive
  :hook ((compilation-mode
          prog-mode
          eshell-mode
          shell-mode) . #'goto-address-mode)
  :bind (:map goto-address-highlight-keymap
              ("<RET>" . goto-address-at-point)
              ("M-<RET>" . newline))
  :commands (goto-address-prog-mode
             goto-address-mode))

(use-package goto-addr
  :hook ((compilation-mode . goto-address-mode)
         (prog-mode . goto-address-prog-mode)
         (elfeed-show-mode . goto-address-mode)
         (eshell-mode . goto-address-mode)
         (shell-mode . goto-address-mode))
  :bind (:map goto-address-highlight-keymap
              ("M-g u" . goto-address-at-point))
  :commands (goto-address-prog-mode
             goto-address-mode))

;;;_ , goto-last-change

(use-package goto-last-change
  :unless noninteractive
  :bind ("M-g m" . goto-last-change))

;;;_ , grab-x-link

(use-package grab-x-link
  :defer t)

;;;_ , grep

(use-package grep+
  :defer t
  :bind (("M-s d" . find-grep-dired)
         ("M-s f" . find-grep)
         ("M-s g" . grep))
  :init
  (progn
    (defun find-grep-in-project (command-args)
      (interactive
       (let ((default (thing-at-point 'symbol)))
         (list (read-shell-command "Run find (like this): "
                                   (cons (concat "git --no-pager grep -n "
                                                 default)
                                         (+ 24 (length default)))
                                   'grep-find-history))))
      (when command-args
        (let ((null-device nil))        ; see grep
          (grep command-args))))

    (bind-key "M-s p" 'find-grep-in-project))

  :config
  (progn
    (grep-apply-setting 'grep-command "egrep -nH -e ")

    (grep-apply-setting
     'grep-find-command
     '("find . -type f -print0 | xargs -P4 -0 egrep -nH -e " . 52))))

(use-package groovy-mode
  :mode ("Jenkinsfile" . groovy-mode))

;;;_ , gtags

(use-package gtags
  :defer t
  :commands gtags-mode
  :diminish gtags-mode
  :config
  (progn
    (defun my-gtags-or-semantic-find-tag ()
      (interactive)
      (if (and (fboundp 'semantic-active-p)
               (funcall #'semantic-active-p))
          (call-interactively #'semantic-complete-jump)
        (call-interactively #'gtags-find-tag)))

    (bind-key "M-." 'my-gtags-or-semantic-find-tag gtags-mode-map)

    (bind-key "C-c t ." 'gtags-find-rtag)
    (bind-key "C-c t f" 'gtags-find-file)
    (bind-key "C-c t p" 'gtags-parse-file)
    (bind-key "C-c t g" 'gtags-find-with-grep)
    (bind-key "C-c t i" 'gtags-find-with-idutils)
    (bind-key "C-c t s" 'gtags-find-symbol)
    (bind-key "C-c t r" 'gtags-find-rtag)
    (bind-key "C-c t v" 'gtags-visit-rootdir)

    (bind-key "<mouse-2>" 'gtags-find-tag-from-here gtags-mode-map)

    (use-package helm-gtags
      :disabled t
      :bind ("M-T" . helm-gtags-select)
      :config
      (bind-key "M-," 'helm-gtags-resume gtags-mode-map))))

;;;_ , gud

(use-package gud
  :commands gud-gdb
  :init
  (progn
    (defun show-debugger ()
      (interactive)
      (let ((gud-buf
             (catch 'found
               (dolist (buf (buffer-list))
                 (if (string-match "\\*gud-" (buffer-name buf))
                     (throw 'found buf))))))
        (if gud-buf
            (switch-to-buffer-other-window gud-buf)
          (call-interactively 'gud-gdb)))))

  :config
  (progn
    (bind-key "<f9>" 'gud-cont)
    (bind-key "<f10>" 'gud-next)
    (bind-key "<f11>" 'gud-step)
    (bind-key "S-<f11>" 'gud-finish)))

;;;_ , haskell-mode

(use-package haskell-mode
  :config)

;;;_ , header2

(use-package header2
  ;; TBD no package but exists on emacsmirror
  :disabled t)

;; ;;;_ , helm

;; ;; (defvar helm-alive-p nil)

;; ;; (use-package helm-config
;; ;;
;; ;;   :init
;; ;;   (progn
;; ;;     (bind-key "C-c M-x" 'helm-M-x)
;; ;;     (bind-key "C-h a" 'helm-c-apropos)
;; ;;     (bind-key "M-s a" 'helm-do-grep)
;; ;;     (bind-key "M-s b" 'helm-occur)
;; ;;     (bind-key "M-s F" 'helm-for-files)

;; ;;     (use-package helm-commands)

;; ;;     (use-package helm-flycheck
;; ;;       :bind ("C-c ! h" . helm-flycheck))

;; ;;     (bind-key "C-h e a" 'my-helm-apropos)
;; ;;     (bind-key "C-x M-!" 'helm-command-from-zsh)
;; ;;     (bind-key "C-x f" 'helm-find-git-file)

;; ;;     (use-package helm-descbinds
;; ;;       :commands helm-descbinds
;; ;;       :init
;; ;;       (fset 'describe-bindings 'helm-descbinds))

;; ;;     (bind-key "C-h b" 'helm-descbinds))

;; ;;   :config
;; ;;   (helm-match-plugin-mode t))

;; ;;;_ , help-mode

;; (use-package help-mode+
;;   :defer t
;;   :bind (:map help-mode-map
;;               ("<tab>" . forward-button)))

;; ;;;_ , helpful

(use-package helpful
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)
   ("C-c C-d" . helpful-at-point)
   ("C-h F" . helpful-function)
   ("C-h C" . helpful-command)))

;;;_ , hi-lock

(use-package hi-lock
  :straight nil
  :bind (("M-o l" . highlight-lines-matching-regexp)
         ("M-o r" . highlight-regexp)
         ("M-o w" . highlight-phrase)))

;;;_ , hilit-chg

(require 'hilit-chg)
(global-highlight-changes-mode t)
(set-face-attribute 'highlight-changes nil :background "gray23" :foreground 'unspecified)
(use-package hilit-chg
  :bind ("M-o C" . highlight-changes-mode))

;;;_ , highlight-sexp

;; (use-package highlight-sexp
;;   :disabled t)

;;;_ , hippie-expand

(use-package hippie-exp
  :unless noninteractive
  :after yasnippet
  :preface
  (global-subword-mode +1)
  :init
  (defun my-yas-hippie-try-expand (first-time)
    (if (not first-time)
        (yas-expand)
      (undo 1)
      nil))

  (defun he-tag-beg ()
    (let ((p
           (save-excursion
             (backward-word 1)
             (point))))
      p))

  (defun try-expand-tag (old)
    (unless  old
      (he-init-string (he-tag-beg) (point))
      (setq he-expand-list
            (sort
             (all-completions he-search-string
                              'tags-complete-tag) 'string-lessp)))
    (while (and he-expand-list
                (he-string-member (car he-expand-list) he-tried-table))
      (setq he-expand-list (cdr he-expand-list)))
    (if (null he-expand-list)
        (progn
          (when old (he-reset-string))
          ())
      (he-substitute-string (car he-expand-list))
      (setq he-expand-list (cdr he-expand-list))
      t))

  (defun my-try-expand-dabbrev-visible (old)
    (save-excursion (try-expand-dabbrev-visible old)))

  :bind (("M-/"   . hippie-expand)
         ("C-M-/" . dabbrev-completion))
  :custom
  (hippie-expand-try-functions-list
   '(my-yas-hippie-try-expand
     my-try-expand-dabbrev-visible
     try-expand-dabbrev
     try-expand-dabbrev-all-buffers
     try-expand-dabbrev-from-kill
     try-expand-tag
     try-expand-flexible-abbrev
     try-complete-file-name-partially
     try-complete-file-name
     try-expand-all-abbrevs
     try-expand-list
     try-expand-line
     try-expand-line-all-buffers
     try-complete-lisp-symbol-partially
     try-complete-lisp-symbol)))

(use-package hl-indent-scope
  :disabled t
  :commands (hl-indent-scope-mode)
  :hook ((c-mode
          c++-mode
          clojure-mode
          clojurescript-mode
          clojurec-mode
          cmake-mode
          python-mode
          emacs-lisp-mode) . hl-indent-scope-mode))

(use-package hl-line
  :config
  (global-hl-line-mode +1))

;;;_ , htmlize

;; (use-package htmlize
;;   )

;;;_ , hydra

(use-package hydra
  :config (setq hydra-hint-display-type 'posframe))

;;;_ , hyperbole

(use-package hyperbole
  :disabled t
  :defer 10
  :bind* (("C-M-." . hkey-either)
          ("A-<return>" . hkey-operate))
  :init
  (setq hbmap:dir-user (expand-file-name "hyperb" user-data-directory))
  :config
  (when (eq temp-buffer-show-function #'hkey-help-show)
    (setq temp-buffer-show-function nil))
  (remove-hook 'temp-buffer-show-hook #'hkey-help-show)

  (defact visit-haskell-definition ()
    "Go to the definition of a symbol in Haskell."
    (interactive)
    (condition-case err
        (call-interactively #'haskell-mode-jump-to-def-or-tag)
      (error
       (call-interactively #'dumb-jump-go))))

  (defib haskell-definition-link ()
    "Go to the definition of a symbol in Haskell."
    (and (eq major-mode 'haskell-mode)
         (hact #'visit-haskell-definition)))

  (defib gnus-article-urls-link ()
    "Visit the URLs in a Gnus article."
    (and (eq major-mode 'gnus-summary-mode)
         (hact #'gnus-article-browse-urls))))

;;;_ , ibuffer

(use-package ibuffer-projectile
  :disabled t
  :defer t
  :bind ("C-x C-b" . ibuffer)
  :config
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-projectile-set-filter-groups)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))

;;;_ , ibuffer

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :custom
  (ibuffer-default-display-maybe-show-predicates t)
  (ibuffer-expert t)
  (ibuffer-formats
   '((mark modified read-only " "
           (name 16 -1)
           " "
           (size 6 -1 :right)
           " "
           (mode 16 16)
           " " filename)
     (mark " "
           (name 16 -1)
           " " filename)))
  (ibuffer-maybe-show-regexps nil)
  (ibuffer-saved-filter-groups
   '(("default"
      ("Magit"
       (or
        (mode . magit-status-mode)
        (mode . magit-log-mode)
        (name . "\\*magit")
        (name . "magit-")
        (name . "git-monitor")))
      ("Coq"
       (or
        (mode . coq-mode)
        (name . "\\<coq\\>")
        (name . "_CoqProject")))
      ("Commands"
       (or
        (mode . shell-mode)
        (mode . eshell-mode)
        (mode . term-mode)
        (mode . compilation-mode)))
      ("Haskell"
       (or
        (mode . haskell-mode)
        (mode . haskell-cabal-mode)
        (mode . haskell-literate-mode)))
      ("Rust"
       (or
        (mode . rust-mode)
        (mode . cargo-mode)
        (name . "\\*Cargo")
        (name . "^\\*rls\\(::stderr\\)?\\*")
        (name . "eglot")))
      ("Nix"
       (mode . nix-mode))
      ("C++"
       (or
        (mode . c-mode)
        (mode . c++-mode)))
      ("Lisp"
       (mode . emacs-lisp-mode))
      ("Dired"
       (mode . dired-mode))
      ("Gnus"
       (or
        (mode . message-mode)
        (mode . mail-mode)
        (mode . gnus-group-mode)
        (mode . gnus-summary-mode)
        (mode . gnus-article-mode)
        (name . "^\\.newsrc-dribble")
        (name . "^\\*\\(sent\\|unsent\\|fetch\\)")
        (name . "^ \\*\\(nnimap\\|nntp\\|nnmail\\|gnus\\|server\\|mm\\*\\)")
        (name . "\\(Original Article\\|canonical address\\|extract address\\)")))
      ("Org"
       (or
        (name . "^\\*Calendar\\*$")
        (name . "^\\*Org Agenda")
        (name . "^ \\*Agenda")
        (name . "^diary$")
        (mode . org-mode)))
      ("Emacs"
       (or
        (name . "^\\*scratch\\*$")
        (name . "^\\*Messages\\*$")
        (name . "^\\*\\(Customize\\|Help\\)")
        (name . "\\*\\(Echo\\|Minibuf\\)"))))))
  (ibuffer-show-empty-filter-groups nil)
  (ibuffer-shrink-to-minimum-size t t)
  (ibuffer-use-other-window t)
  :init
  (add-hook 'ibuffer-mode-hook
            #'(lambda ()
                (ibuffer-switch-to-saved-filter-groups "default"))))

(use-package iedit
  :bind (("C-x ," . iedit-mode)))

(provide 'init-iedit)
;;;_ , ido

(use-package ido
  ;; disabled in favor of ivy-mode
  :disabled t
  :defines (ido-cur-item
            ido-require-match
            ido-selected
            ido-final-text
            ido-show-confirm-message)
  :init
  (ido-mode 'buffer)

  :config
  (progn
    (use-package ido-hacks
      :config
      (ido-hacks-mode 1))

    (use-package ido-springboard)

    (defun ido-smart-select-text ()
      "Select the current completed item.  Do NOT descend into directories."
      (interactive)
      (when (and (or (not ido-require-match)
                     (if (memq ido-require-match
                               '(confirm confirm-after-completion))
                         (if (or (eq ido-cur-item 'dir)
                                 (eq last-command this-command))
                             t
                           (setq ido-show-confirm-message t)
                           nil))
                     (ido-existing-item-p))
                 (not ido-incomplete-regexp))
        (when ido-current-directory
          (setq ido-exit 'takeprompt)
          (unless (and ido-text (= 0 (length ido-text)))
            (let ((match (ido-name (car ido-matches))))
              (throw 'ido
                     (setq ido-selected
                           (if match
                               (replace-regexp-in-string "/\\'" "" match)
                             ido-text)
                           ido-text ido-selected
                           ido-final-text ido-text)))))
        (exit-minibuffer)))

    (add-hook 'ido-minibuffer-setup-hook
              #'(lambda ()
                  (bind-key "<return>" 'ido-smart-select-text
                            ido-file-completion-map)))

    (defun ido-switch-buffer-tiny-frame (buffer)
      (interactive (list (ido-read-buffer "Buffer: " nil t)))
      (with-selected-frame
          ;; (make-frame '((width                . 80)
          ;;               (height               . 22)
          ;;               (left-fringe          . 0)
          ;;               (right-fringe         . 0)
          ;;               (vertical-scroll-bars . nil)
          ;;               (unsplittable         . t)
          ;;               (has-modeline-p       . nil)
          ;;               ;;(background-color     . "grey80")
          ;;               (minibuffer           . nil)))
          (switch-to-buffer buffer)
        (set (make-local-variable 'mode-line-format) nil)))

    (bind-key "C-x 5 t" 'ido-switch-buffer-tiny-frame)))

;;;_ , ielm

(use-package ielm
  :commands ielm
  :bind (:map ielm-map ("<return>" . my-ielm-return))
  :config
  (defun my-ielm-return ()
    (interactive)
    (let ((end-of-sexp (save-excursion
                         (goto-char (point-max))
                         (skip-chars-backward " \t\n\r")
                         (point))))
      (if (>= (point) end-of-sexp)
          (progn
            (goto-char (point-max))
            (skip-chars-backward " \t\n\r")
            (delete-region (point) (point-max))
            (call-interactively #'ielm-return))
        (call-interactively #'paredit-newline)))))

;;;_ , iflipb

(use-package iflipb
  :disabled t
  :commands (iflipb-next-buffer iflipb-previous-buffer)
  :bind (("S-<right>" . my-iflipb-next-buffer)
         ("S-<left>" . my-iflipb-previous-buffer))
  :init
  (progn
    (defvar my-iflipb-auto-off-timeout-sec 2)
    (defvar my-iflipb-auto-off-timer-canceler-internal nil)
    (defvar my-iflipb-ing-internal nil)

    (defun my-iflipb-auto-off ()
      (message nil)
      (setq my-iflipb-auto-off-timer-canceler-internal nil
            my-iflipb-ing-internal nil))

    (defun my-iflipb-next-buffer (arg)
      (interactive "P")
      (iflipb-next-buffer arg)
      (if my-iflipb-auto-off-timer-canceler-internal
          (cancel-timer my-iflipb-auto-off-timer-canceler-internal))
      (run-with-idle-timer my-iflipb-auto-off-timeout-sec 0 'my-iflipb-auto-off)
      (setq my-iflipb-ing-internal t))

    (defun my-iflipb-previous-buffer ()
      (interactive)
      (iflipb-previous-buffer)
      (if my-iflipb-auto-off-timer-canceler-internal
          (cancel-timer my-iflipb-auto-off-timer-canceler-internal))
      (run-with-idle-timer my-iflipb-auto-off-timeout-sec 0 'my-iflipb-auto-off)
      (setq my-iflipb-ing-internal t)))

  :config
  (progn
    (setq iflipb-always-ignore-buffers
          "\\`\\( \\|diary\\|ipa\\|\\.newsrc-dribble\\'\\)"
          iflipb-wrap-around t)

    (defun iflipb-first-iflipb-buffer-switch-command ()
      "Determines whether this is the first invocation of
iflipb-next-buffer or iflipb-previous-buffer this round."
      (not (and (or (eq last-command 'my-iflipb-next-buffer)
                    (eq last-command 'my-iflipb-previous-buffer))
                my-iflipb-ing-internal)))))

;;;_ , image-file

(use-package image-file
  :straight nil
  :init
  (auto-image-file-mode 1))

(use-package inf-clojure
  :init
  (defun figwheel ()
    (interactive)
    (run-clojure "lein figwheel"))
  :hook ('clojurescript-mode-hook . #'inf-clojure-minor-mode))

;;;_ , impatient-mode

(use-package impatient-mode
  :defer t
  :config
  (progn
    (defvar imnpatient-mode-initialized nil)

    (defun initialize-impatient-mode ()
      (unless impatient-mode-initialized)
      (setq impatient-mode-initialized t)

      (use-package simple-httpd
        :defer t)

      (httpd-start))
    (initialize-impatient-mode)))

;;;_ , info

(use-package info
  :autoload Info-goto-node)

(use-package info-look
  :init
  :autoload info-lookup-add-help)

;; (use-package info-lookmore
;;   :disabled t
;;   :after info-look
;;   :config
;;   (info-lookmore-elisp-cl)
;;   (info-lookmore-elisp-userlast)
;;   (info-lookmore-elisp-gnus)
;;   (info-lookmore-apropos-elisp))

;; ;; (use-package info-look
;; ;;   :commands info-lookup-add-help)

(use-package isearch
  :straight nil
  :no-require t
  :bind (("C-M-r" . isearch-backward-other-window)
         ("C-M-s" . isearch-forward-other-window))
  :bind (:map isearch-mode-map
              ("C-c" . isearch-toggle-case-fold)
              ("C-t" . isearch-toggle-regexp)
              ("C-^" . isearch-edit-string)
              ("C-i" . isearch-complete))
  :preface
  (defun isearch-backward-other-window ()
    (interactive)
    (split-window-vertically)
    (other-window 1)
    (call-interactively 'isearch-backward))

  (defun isearch-forward-other-window ()
    (interactive)
    (split-window-vertically)
    (other-window 1)
    (call-interactively 'isearch-forward)))

(use-package jira-markup-mode
  :commands (jira-markup-mode)
  :config
  (add-hook 'jira-markup-mode-hook
            (lambda ()
              (word-wrap t)))
  (add-hook 'jira-markup-mode-hook #'turn-on-orgtbl))

(use-package journalctl-mode)

;;;_ , JS-mode

(use-package js2-mode
  :mode "\\.js\\'")

(use-package julia-mode
  :defer t
  :commands julia-mode
  :mode ("\\.jl$" . julia-mode)
  :init
  (progn
    (autoload 'julia-mode "julia-mode" nil t)
    (setq inferior-julia-program-name "/usr/bin/julia")
    )
  :config
  (progn
    (add-to-list 'julia-mode-hook 'cg/modify-current-syntax-table)
    (setq inferior-julia-program-name "/usr/bin/julia")
    (add-to-list 'julia-mode-hook 'cg/command-line-keybindings)
    ;; (add-to-list 'inferior-ess-mode-hook 'cg/command-line-keybindings)
    )
  )

(use-package ess-julia.el
  :disabled t
  :defer t
  :commands julia
  :init                                 ; run before actual loading
  (progn
    (autoload 'julia "ess-julia.el" nil t)
    (setq inferior-julia-program-name "/usr/bin/julia")
    )
  :config
  (progn
    (require 'ess-site)
    (setq inferior-julia-program-name "/usr/bin/julia")
    (setq ess-tracebug-prefix "\M-c") ; define debug-mode starting key
    (setq ess-use-tracebug t)         ; tracebug is called for R
                                        ; AND JULIA!!
    (setq ess-tracebug-inject-source-p t)
    (add-to-list 'julia-mode-hook 'cg/command-line-keybindings)
    ;; (add-to-list 'inferior-ess-mode-hook 'cg/command-line-keybindings)
    )
  )

(use-package julia-snail
  :after julia-mode
  :hook (julia-mode . julia-snail-mode)
  :config
  (progn
    ;; order matters, unfortunately:
    (add-to-list 'display-buffer-alist
                 ;; match buffers named "*julia" in general
                 '("\\*julia"
                   ;; actions:
                   (display-buffer-reuse-window display-buffer-same-window)))
    (add-to-list 'display-buffer-alist
                 ;; when displaying buffers named "*julia" in REPL mode
                 '((lambda (bufname _action)
                     (and (string-match-p "\\*julia" bufname)
                          (with-current-buffer bufname
                            (bound-and-true-p julia-snail-repl-mode))))
                   ;; actions:
                   (display-buffer-reuse-window display-buffer-pop-up-window)))))

(use-package just-mode
  :unless noninteractive)

;;;_ , key-chord

(use-package key-chord
  ;; BULK-ENSURE :ensure t
  :defer t
  :commands key-chord-mode
  :init
  (key-chord-mode 1)
  :config
  (setq key-chord-two-keys-delay 0.1))

(use-package keypression
  :commands key-chord-mode)

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default) ; Have background color be the same as `corfu' face background
  (kind-icon-blend-background nil) ; Use midpoint color between foreground and background colors ("blended")?
  (kind-icon-blend-frac 0.08)

  ;; NOTE 2022-02-05: `kind-icon' depends `svg-lib' which creates a cache
  ;; directory that defaults to the `user-emacs-directory'. Here, I change that
  ;; directory to a location appropriate to `no-littering' conventions, a
  ;; package which moves directories of other packages to sane locations.
  (svg-lib-icons-dir (no-littering-expand-var-file-name "svg-lib/cache/")) ; Change cache dir
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter) ; Enable `kind-icon'

  ;; Add hook to reset cache so the icon colors match my theme
  ;; NOTE 2022-02-05: This is a hook which resets the cache whenever I switch
  ;; the theme using my custom defined command for switching themes. If I don't
  ;; do this, then the backgound color will remain the same, meaning it will not
  ;; match the background color corresponding to the current theme. Important
  ;; since I have a light theme and dark theme I switch between. This has no
  ;; function unless you use something similar
  (add-hook 'kb/themes-hooks #'(lambda () (interactive) (kind-icon-reset-cache))))

;;;_ , kotlin-mode

(use-package kotlin-mode
  ;; BULK-ENSURE :ensure t
  :mode "\\.kt\\'")

;;;_ , kubernetes

(use-package kubel
  :defer t
  :commands (kubel))

(use-package kubernetes
  :defer t
  :commands (kubernetes-overview))

;;;_ , ledger

(use-package ledger-mode
  ;; BULK-ENSURE :ensure t
  :defer t
  :mode "\\.ledger\\'"
  :config
  (define-key ledger-mode-map (kbd "C-c c") 'ledger-mode-clean-buffer)
  (setq ledger-post-amount-alignment-at :decimal
        ledger-post-amount-alignment-column 49
        ledger-clear-whole-transactions t)
  :init
  (progn
    (defun my-ledger-start-entry (&optional arg)
      (interactive "p")
      (find-file-other-window "~/Documents/Accounts/ledger.dat")
      (goto-char (point-max))
      (skip-syntax-backward " ")
      (if (looking-at "\n\n")
          (goto-char (point-max))
        (delete-region (point) (point-max))
        (insert ?\n)
        (insert ?\n))
      (insert (format-time-string "%Y/%m/%d ")))

    (bind-key "C-c L" 'my-ledger-start-entry)

    (defun ledger-matchup ()
      (interactive)
      (while (re-search-forward "\\(\\S-+Unknown\\)\\s-+\\$\\([-,0-9.]+\\)"
                                nil t)
        (let ((account-beg (match-beginning 1))
              (account-end (match-end 1))
              (amount (match-string 2))
              account answer)
          (goto-char account-beg)
          (set-window-point (get-buffer-window) (point))
          (recenter)
          (redraw-display)
          (with-current-buffer (get-buffer "nrl-mastercard-old.dat")
            (goto-char (point-min))
            (when (re-search-forward (concat "\\(\\S-+\\)\\s-+\\$" amount)
                                     nil t)
              (setq account (match-string 1))
              (goto-char (match-beginning 1))
              (set-window-point (get-buffer-window) (point))
              (recenter)
              (redraw-display)
              (setq answer
                    (read-char (format "Is this a match for %s (y/n)? "
                                       account)))))
          (when (eq answer ?y)
            (goto-char account-beg)
            (delete-region account-beg account-end)
            (insert account))
          (forward-line))))))

(use-package flycheck-ledger
  :after ledger-mode)

;;;_ , lice

(use-package lice
  :defer t)

(use-package lin
  :config
  (lin-global-mode +1))

;;;_ , lisp-mode

(use-package lisp-mode
  :straight nil
  :hook (emacs-lisp-mode .
                         (lambda () (add-hook 'after-save-hook 'check-parens nil t)))
  :hook (lisp-mode . (lambda () (add-hook 'after-save-hook 'check-parens nil t)))
  :init
  (dolist (mode '(ielm-mode
                  inferior-emacs-lisp-mode
                  inferior-lisp-mode
                  lisp-interaction-mode
                  lisp-mode
                  emacs-lisp-mode))
    (font-lock-add-keywords
     mode
     '(("(\\(lambda\\)\\>"
        (0 (ignore
            (compose-region (match-beginning 1)
                            (match-end 1) ?λ))))
       ("(\\(ert-deftest\\)\\>[         '(]*\\(setf[    ]+\\sw+\\|\\sw+\\)?"
        (1 font-lock-keyword-face)
        (2 font-lock-function-name-face
           nil t))))))

(use-package lispy
  :unless noninteractive
  :custom
  (lispy-compat '(edebug cider))
  :hook ((emacs-lisp-mode
          clojure-mode
          clojurescript-mode
          lisp-mode
          scheme-mode) . #'lispy-mode))

(use-package lively
  :bind ("C-x M-e" . lively));(current-time-string)

(use-package loopy
  :config
  (require 'loopy-iter)
  (require 'loopy-pcase)
  (require 'loopy-seq))

(use-package loopy-dash
  :after (loopy)
  :demand t)

(use-package edebug)

(use-package eldoc
  :diminish
  :custom
  (eldoc-echo-area-use-multiline-p nil)
  :hook (prog-mode . eldoc-mode))

(use-package eldoc-overlay
  :disabled t ;; too visually jarring.
  :diminish ""
  :config
  (global-eldoc-overlay-mode +1))

(use-package cldoc
  :commands (cldoc-mode turn-on-cldoc-mode)
  :diminish cldoc-mode)

(use-package elisp-depend
  :commands elisp-depend-print-dependencies)

(use-package elisp-docstring-mode
  :commands elisp-docstring-mode)

(use-package elisp-slime-nav
  :diminish
  :commands (elisp-slime-nav-mode
             elisp-slime-nav-find-elisp-thing-at-point))

(use-package elmacro
  :bind (("C-c M" . elmacro-mode)
         ("C-x C-)" . elmacro-show-last-macro)))

(use-package ert
  :commands ert-run-tests-interactively
  :bind ("C-c e t" . ert-run-tests-interactively))

(use-package elint
  :commands 'elint-initialize
  :init
  (defun elint-current-buffer ()
    (interactive)
    (elint-initialize)
    (elint-current-buffer))

  :config
  (progn
    (add-to-list 'elint-standard-variables 'current-prefix-arg)
    (add-to-list 'elint-standard-variables 'command-line-args-left)
    (add-to-list 'elint-standard-variables 'buffer-file-coding-system)
    (add-to-list 'elint-standard-variables 'emacs-major-version)
    (add-to-list 'elint-standard-variables 'window-system)))

(use-package sotlisp)

;;;_ , llvm-mode

(use-package llvm-mode
  :disabled t
  :mode ("\\.ll\\'" . llvm-mode))

;;;_ , log4j-mode

(use-package log4j-mode
  :disabled t
  :mode ("\\.log\\'" . log4j-mode))

(use-package lsp-java
  :disabled t)

(use-package lsp-treemacs
  :disabled t)

(use-package lsp-ui
  :commands lsp-ui-mode)

;;;_ , lua-mode

(use-package lua-mode
  :defer t
  :config
  (progn
    (add-hook 'lua-mode-hook
              (lambda ()))
    (setq lua-indent-level 2
          lua-documentation-url
          "http://www.lua.org/manual/5.3/manual.html"
          lua-default-application "lua5.3")
    (add-to-list 'auto-mode-alist '("\\.nse$" . lua-mode))))

;;;_ , macrostep

(use-package macrostep
  :defer t
  :bind ("C-c e m" . macrostep-expand))

;;;_ , magit

(use-package magit
  :unless noninteractive
  :init
  (setq magit-last-seen-setup-instructions "1.4.0")
  :bind ("C-x g" . magit-status)
  :custom
  (magit-process-find-password-functions '(magit-process-password-auth-source))
  :config
  (progn
    (setq magit-diff-refine-hunk 'all)
    (global-git-commit-mode t)
    (setq magit-push-always-verify nil)
    (setenv "GIT_PAGER" "")

    (add-hook 'magit-log-edit-mode-hook
              #'(lambda ()
                  (set-fill-column 72)
                  (flyspell-mode)))

    (defun start-git-monitor ()
      (interactive)
      (start-process "git-monitor" (current-buffer) "~/bin/git-monitor"))))

(use-package magit-todos
  :after magit)

(use-package magit-topgit
  :after magit
  :defer t)

(use-package magit-filenotify
  :after magit)

(use-package magithub
  :disabled t
  :unless noninteractive
  :after magit
  :config
  (magithub-feature-autoinject t)
  :custom
  (magithub-clone-default-directory "~"))

;;;_ , markdown-mode

(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode)
  :defer t)

;; (use-package markdown-preview-mode
;;   :after markdown-mode
;;   :config
;;   (setq markdown-preview-stylesheets
;;         (list (concat "https://github.com/dmarcotte/github-markdown-preview/"
;;                       "blob/master/data/css/github.css"))))

(use-package mastodon
  :config
  (mastodon-discover))

;;;_ , meghanada

(use-package meghanada
  ;; BULK-ENSURE :ensure t
  :defer t
  :commands meghanada-mode
  :init
  (add-hook 'java-mode-hook
            #'(lambda ()
                (meghanada-mode t)
                (flycheck-mode t)))
  :custom
  (meghanada-use-company t)
  (meghanada-use-flycheck t)
  (meghanada-use-auto-start t)

  :config
  (bind-key "M-g d" #'meghanada-jump-declaration meghanada-mode-map)
  (unless (f-exists? (meghanada--locate-server-jar))
    (meghanada-install-server)))

;; ;;;_ , minimap

(use-package minimap
  :defer t
  :bind ("M-o m" . minimap-mode))

(use-package moccur-edit
  :after color-moccur)

(use-package move-lines
  :straight nil
  :no-require t
  :demand t
  :bind (([(meta shift up)]   . my/move-line-up)
         ([(meta shift down)] . my/move-line-down))
  :preface
  (defun my/move-line-up ()
    (interactive)
    (transpose-lines 1)
    (previous-line 2))
  (defun my/move-line-down ()
    (interactive)
    (next-line 1)
    (transpose-lines 1)
    (previous-line 1)))

;;;_ , mule

(use-package mule
  :straight nil
  :init
  (progn
    (prefer-coding-system 'utf-8)
    (set-terminal-coding-system 'utf-8)
    (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))))

;;;_ , multi-term

(use-package multi-term
  :bind (("C-c t" . multi-term-next)
         ("C-c T" . multi-term))
  :custom
  (multi-term-program "tmux")
  (multi-term-program-switches "-CC")
  (multi-term-scroll-show-maximum-output t)
  (term-bind-key-alist
   '(("C-c C-c" . term-interrupt-subjob)
     ("C-b"     . my-term-send-raw-at-prompt)
     ("C-f"     . my-term-send-raw-at-prompt)
     ("C-a"     . my-term-send-raw-at-prompt)
     ("C-e"     . my-term-send-raw-at-prompt)
     ("C-p"     . previous-line)
     ("C-n"     . next-line)
     ("C-s"     . isearch-forward)
     ("C-r"     . isearch-backward)
     ("C-m"     . term-send-raw)
     ("M-f"     . term-send-forward-word)
     ("M-b"     . term-send-backward-word)
     ("M->"     . my-term-end-of-buffer)
     ("M-o"     . term-send-backspace)
     ("M-p"     . term-send-up)
     ("M-n"     . term-send-down)
     ("M-d"     . term-send-forward-kill-word)
     ("M-DEL"   . term-send-backward-kill-word)
     ("M-r"     . term-send-reverse-search-history)
     ("M-,"     . term-send-input)
     ("M-/"     . comint-dynamic-complete)
     ("C-y"     . term-paste)))
  :init
  (defun screen ()
    (interactive)
    (let ((term-buffer
           (let ((multi-term-program (executable-find "screen"))
                 (multi-term-program-switches "-DR"))
             (multi-term-get-buffer))))
      (set-buffer term-buffer)
      (multi-term-internal)
      (switch-to-buffer term-buffer)))

  :config
  (require 'term)

  (defalias 'my-term-send-raw-at-prompt 'term-send-raw)

  (defun my-term-end-of-buffer ()
    (interactive)
    (call-interactively #'end-of-buffer)
    (if (and (eobp) (bolp))
        (delete-char -1)))

  (defadvice term-process-pager (after term-process-rebind-keys activate)
    (define-key term-pager-break-map  "\177" 'term-pager-back-page)))

(use-package multi-vterm
  :bind ("C-<f9>" . multi-vterm)
  :custom (multi-vterm-buffer-name "vterm")
  :config
  (with-no-warnings
    ;; Use `pop-to-buffer' instead of `switch-to-buffer'
    (defun my-multi-vterm ()
      "Create new vterm buffer."
      (interactive)
      (let ((vterm-buffer (multi-vterm-get-buffer)))
        (setq multi-vterm-buffer-list
              (nconc multi-vterm-buffer-list (list vterm-buffer)))
        (set-buffer vterm-buffer)
        (multi-vterm-internal)
        (pop-to-buffer vterm-buffer)))
    (advice-add #'multi-vterm :override #'my-multi-vterm)

    ;; FIXME: `project-root' is introduced in 27+.
    (defun my-multi-vterm-project-root ()
      "Get `default-directory' for project using projectile or project.el."
      (unless (boundp 'multi-vterm-projectile-installed-p)
        (setq multi-vterm-projectile-installed-p (require 'projectile nil t)))
      (if multi-vterm-projectile-installed-p
          (projectile-project-root)
        (let ((project (or (project-current)
                           `(transient . ,default-directory))))
          (if (fboundp 'project-root)
              (project-root project)
            (cdr project)))))
    (advice-add #'multi-vterm-project-root :override
                #'my-multi-vterm-project-root)))

;;;_ , multiple-cursors

(use-package multiple-cursors
  :after selected
  :defer

  ;; - Sometimes you end up with cursors outside of your view. You can scroll
  ;;   the screen to center on each cursor with `C-v` and `M-v`.
  ;;
  ;; - If you get out of multiple-cursors-mode and yank - it will yank only
  ;;   from the kill-ring of main cursor. To yank from the kill-rings of every
  ;;   cursor use yank-rectangle, normally found at C-x r y.

  :bind (("<C-m> ^"     . mc/edit-beginnings-of-lines)
         ("<C-m> `"     . mc/edit-beginnings-of-lines)
         ("<C-m> $"     . mc/edit-ends-of-lines)
         ("<C-m> '"     . mc/edit-ends-of-lines)
         ("<C-m> R"     . mc/reverse-regions)
         ("<C-m> S"     . mc/sort-regions)
         ("<C-m> W"     . mc/mark-all-words-like-this)
         ("<C-m> Y"     . mc/mark-all-symbols-like-this)
         ("<C-m> a"     . mc/mark-all-like-this-dwim)
         ("<C-m> c"     . mc/mark-all-dwim)
         ("<C-m> l"     . mc/insert-letters)
         ("<C-m> n"     . mc/insert-numbers)
         ("<C-m> r"     . mc/mark-all-in-region)
         ("<C-m> s"     . set-rectangular-region-anchor)
         ("<C-m> %"     . mc/mark-all-in-region-regexp)
         ("<C-m> t"     . mc/mark-sgml-tag-pair)
         ("<C-m> w"     . mc/mark-next-like-this-word)
         ("<C-m> x"     . mc/mark-more-like-this-extended)
         ("<C-m> y"     . mc/mark-next-like-this-symbol)
         ("<C-m> C-x"   . reactivate-mark)
         ("<C-m> C-SPC" . mc/mark-pop)
         ("<C-m> ("     . mc/mark-all-symbols-like-this-in-defun)
         ("<C-m> C-("   . mc/mark-all-words-like-this-in-defun)
         ("<C-m> M-("   . mc/mark-all-like-this-in-defun)
         ("<C-m> ["     . mc/vertical-align-with-space)
         ("<C-m> {"     . mc/vertical-align)

         ("S-<down-mouse-1>")
         ("S-<mouse-1>" . mc/add-cursor-on-click))

  :bind (:map selected-keymap
              ("c"   . mc/edit-lines)
              ("."   . mc/mark-next-like-this)
              ("<"   . mc/unmark-next-like-this)
              ("C->" . mc/skip-to-next-like-this)
              (","   . mc/mark-previous-like-this)
              (">"   . mc/unmark-previous-like-this)
              ("C-<" . mc/skip-to-previous-like-this)
              ("y"   . mc/mark-next-symbol-like-this)
              ("Y"   . mc/mark-previous-symbol-like-this)
              ("w"   . mc/mark-next-word-like-this)
              ("W"   . mc/mark-previous-word-like-this))

  :preface
  (defun reactivate-mark ()
    (interactive)
    (activate-mark)))

(use-package mc-calc
  :after multiple-cursors
  :bind (("<C-m> = c" . mc-calc)
         ("<C-m> = =" . mc-calc-eval)
         ("<C-m> = g" . mc-calc-grab)
         ("<C-m> = b" . mc-calc-copy-to-buffer)))

(use-package mc-extras
  :after multiple-cursors
  :bind (("<C-m> M-C-f" . mc/mark-next-sexps)
         ("<C-m> M-C-b" . mc/mark-previous-sexps)
         ("<C-m> <"     . mc/mark-all-above)
         ("<C-m> >"     . mc/mark-all-below)
         ("<C-m> C-d"   . mc/remove-current-cursor)
         ("<C-m> C-k"   . mc/remove-cursors-at-eol)
         ("<C-m> M-d"   . mc/remove-duplicated-cursors)
         ("<C-m> |"     . mc/move-to-column)
         ("<C-m> ~"     . mc/compare-chars)))

(use-package mc-freeze
  :straight nil
  :after multiple-cursors
  :bind ("<C-m> f" . mc/freeze-fake-cursors-dwim))

(use-package mc-rect
  :straight nil
  :after multiple-cursors
  :bind ("<C-m> ]" . mc/rect-rectangle-to-multiple-cursors))

(use-package native-complete
  :config
  (add-to-list 'completion-at-point-functions 'native-complete-at-point)
  (with-eval-after-load 'shell
    (native-complete-setup-bash)))

;;;_ , nroff-mode

(use-package nroff-mode
  :commands nroff-mode
  :config
  (progn
    (defun update-nroff-timestamp ()
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^\\.Dd ")
          (let ((stamp (format-time-string "%B %e, %Y")))
            (unless (looking-at stamp)
              (delete-region (point) (line-end-position))
              (insert stamp)
              (let (after-save-hook)
                (save-buffer)))))))

    (add-hook 'nroff-mode-hook
              #'(lambda ()
                  (add-hook 'after-save-hook 'update-nroff-timestamp nil t)))))

;;;_ , nxml-mode

(use-package nxml-mode
  :straight nil
  :commands nxml-mode
  :init
  (defalias 'xml-mode 'nxml-mode)
  :config
  (progn
    (defun my-nxml-mode-hook ()
      (bind-key "<return>" 'newline-and-indent nxml-mode-map))

    (add-hook 'nxml-mode-hook 'my-nxml-mode-hook)

    (defun tidy-xml-buffer ()
      (interactive)
      (save-excursion
        (call-process-region (point-min) (point-max) "tidy" t t nil
                             "-xml" "-i" "-wrap" "0" "-omit" "-q")))

    (bind-key "C-H" 'tidy-xml-buffer nxml-mode-map)))

;; ;;;_ , orca

(use-package omnisharp
  :custom
  (omnisharp-company-sort-results t)
  (omnisharp-auto-complete-want-documentation nil)
  (omnisharp-company-strip-trailing-brackets nil)
  (omnisharp-company-do-template-completion t)
  :config
  (add-to-list 'company-backends #'company-omnisharp)
  (add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode)))

(use-package orderless
  :custom
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion))))
  :init
  (add-to-list 'completion-styles 'substring)
  (add-to-list 'completion-styles 'orderless))

;;;_ , outline

(use-package outline
  :commands outline-minor-mode
  :hook ((LaTeX-mode
          prog-mode) . #'outline-minor-mode))

(use-package outorg
  :after org
  :defer t
  :bind ("C-c C-'" . outorg-edit-as-org)
  :config
  (require 'outshine))

;;;_ , outshine

(use-package outshine
  :after outorg
  :config
  (outshine-mode +1)
  (use-package navi-mode))

;;;_ , pabbrev

(use-package pabbrev
  :commands pabbrev-mode
  :diminish pabbrev-mode)

;;;_ ; paradox
(use-package paradox
  :commands paradox-list-packages
  :custom
  (paradox-execute-asynchronously nil)
  ;; (paradox-github-token (cadr (auth-source-user-and-password
  ;;                              "api.github.com"
  ;;                              (concat user-login-name "^paradox"))))
  :config/el-patch
  (defun package-menu-refresh ()
    "Patch package-menu-refresh to work around Malabarba/paradox#175"
    (interactive)
    (unless (derived-mode-p 'package-menu-mode)
      (user-error "The current buffer is not a Package Menu"))
    (when (el-patch-swap
            (and package-menu-async package--downloads-in-progress)
            (and package-menu-async package--downloads-in-progress
                 (seq-difference package--downloads-in-progress
                                 '(paradox--data))))
      (user-error "Package refresh is already in progress, please wait..."))
    (setq package-menu--old-archive-contents package-archive-contents)
    (setq package-menu--new-package-list nil)
    (package-refresh-contents package-menu-async))
  :config
  (paradox-enable))

;;;_ , paredit

(use-package paredit
  :unless noninteractive
  :bind (:map paredit-mode-map
              ("M-S" . paredit-splice-sexp)
              ("M-s"))
  :diminish paredit-mode
  :hook ((clojure-mode
          cider-repl-mode
          lisp-mode
          clojurescript-mode
          inf-clojure-mode
          inferior-emacs-lisp-mode
          emacs-lisp-mode
          lisp-interaction-mode
          python-mode
          json-mode) . #'paredit-mode))

(use-package paredit-eldoc
  :straight nil
  :no-require t
  :after (paredit eldoc)
  :config
  (eldoc-add-command 'paredit-backward-delete
                     'paredit-close-round))

(use-package paredit-everywhere
  :diminish paredit-everywhere-mode
  :config
  (add-hook 'prog-mode-hook 'paredit-everywhere-mode))

;;;_ , paren

(use-package mic-paren
  :config
  (paren-activate))

(use-package paren
  :straight nil
  :init
  (show-paren-mode 1))

;;;_ ; parinfer

(use-package parinfer
  :disabled t
  :unless noninteractive
  :bind
  (("C-|" . parinfer-toggle-mode))
  :hook (clojure-mode . #'parinfer-mode)
  :hook (cider-repl-mode . #'parinfer-mode)
  :hook (lisp-mode . #'parinfer-mode)
  :hook (inferior-emacs-lisp-mode . #'parinfer-mode)
  :hook (emacs-lisp-mode . #'parinfer-mode)
  :hook (lisp-interaction-mode . #'parinfer-mode)
  :hook (js-mode . #'parinfer-mode)
  :custom
  (parinfer-extensions
   '(defaults       ; should be included.
      pretty-parens  ; different paren styles for different modes.
      ;; evil           ; If you use Evil.
      lispy          ; If you use Lispy. With this extension, you should install Lispy and do not enable lispy-mode directly.
      paredit        ; Introduce some paredit commands.
      smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
      smart-yank)))   ; Yank behavior depend on mode.

;;;_ , pass

(use-package pass
  :straight nil
  :commands (pass pass-view-mode)
  :mode ("\\.passwords/.*\\.gpg\\'" . pass-view-mode)
  :hook (pass-view-mode . #'pass-view--prepare-otp))

(use-package password-store
  :commands (password-store-insert
             password-store-copy
             password-store-get)
  :config
  (defun password-store--run-edit (entry)
    (require 'pass)
    (find-file (concat (expand-file-name entry (password-store-dir)) ".gpg")))

  (defun password-store-insert (entry login password)
    "Insert a new ENTRY containing PASSWORD."
    (interactive (list (read-string "Password entry: ")
                       (read-string "Login: ")
                       (read-passwd "Password: " t)))
    (message "%s" (shell-command-to-string
                   (if (string= "" login)
                       (format "echo %s | %s insert -m -f %s"
                               (shell-quote-argument password)
                               password-store-executable
                               (shell-quote-argument entry))
                     (format "echo -e '%s\nlogin: %s' | %s insert -m -f %s"
                             password login password-store-executable
                             (shell-quote-argument entry)))))))

(use-package password-store-otp
  :defer t
  :config
  (defun password-store-otp-append-from-image (entry)
    "Check clipboard for an image and scan it to get an OTP URI,
append it to ENTRY."
    (interactive (list (read-string "Password entry: ")))
    (let ((qr-image-filename (password-store-otp--get-qr-image-filename entry)))
      (when (not (zerop (call-process "screencapture" nil nil nil
                                      "-T5" qr-image-filename)))
        (error "Couldn't get image from clipboard"))
      (with-temp-buffer
        (condition-case nil
            (call-process "zbarimg" nil t nil "-q" "--raw"
                          qr-image-filename)
          (error
           (error "It seems you don't have `zbar-tools' installed")))
        (password-store-otp-append
         entry
         (buffer-substring (point-min) (point-max))))
      (when (not password-store-otp-screenshots-path)
        (delete-file qr-image-filename)))))

(use-package pcomplete
  :defer t
  :custom
  (pcomplete-compare-entry-function 'file-newer-than-file-p))

;;;_ , persistent-scratch

(use-package my-scratch
  :disabled t
  :after org-mode
  :preface
  (defun my-scratch-save ()
    (message "Saving scratch.org")
    (ignore-errors
      (with-current-buffer "*scratch*"
        (write-region nil nil "~/.emacs.d/var/scratch.org"))))

  (defun my-scratch-restore ()
    (message "Loading scratch.org")
    (let ((f "~/.emacs.d/var/scratch.org"))
      (when (file-exists-p f)
        (with-current-buffer "*scratch*"
          (erase-buffer)
          (insert-file-contents f)))))

  (add-hook 'kill-emacs-hook #'my-scratch-save)
  (add-hook 'after-init-hook #'my-scratch-restore))
;; This is not a real package so don't load it

(use-package persistent-scratch
  :after org-mode
  :unless noninteractive
  :config
  (setq
   persistent-scratch-save-file
   (locate-user-emacs-file "my.org"))
  (persistent-scratch-setup-default))

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :custom
  (pdf-tools-handle-upgrades nil)
  :config
  (dolist
      (pkg
       '(pdf-annot pdf-cache pdf-dev pdf-history pdf-info pdf-isearch
                   pdf-links pdf-misc pdf-occur pdf-outline pdf-sync
                   pdf-util pdf-view pdf-virtual))
    (require pkg))
  (pdf-tools-install))

(use-package per-window-point
  :demand t
  :commands pwp-mode
  :config
  (pwp-mode +1))

;;;_ , perspective

(defvar saved-window-configuration nil)

(defun push-window-configuration ()
  "."
  (interactive)
  (push (current-window-configuration) saved-window-configuration))

(defun pop-window-configuration ()
  "."
  (interactive)
  (let ((config (pop saved-window-configuration)))
    (if config
        (set-window-configuration config)
      (if (> (length (window-list)) 1)
          (delete-window)
        (bury-buffer)))))

(use-package phi-search
  :custom
  (phi-search-limit 100000))

(use-package phi-search-mc
  :after (phi-search multiple-cursors)
  :config
  (phi-search-mc/setup-keys)
  (add-hook 'isearch-mode-mode #'phi-search-from-isearch-mc/setup-keys))

(use-package plantuml-mode
  :mode "\\.plantuml\\'"
  :custom
  (plantuml-default-exec-mode 'jar)
  (plantuml-jar-path "/usr/share/java/plantuml.jar"))

(use-package poporg
  :bind ("C-x C-;" . poporg-dwim))

(use-package posframe)

;;;_ , pp-c-l

(use-package pp-c-l
  :disabled t
  :hook (prog-mode . #'pretty-control-l-mode))

;;;_ , predictive-mode
;; company-statistics seems good enough?

;; (use-package predictive)

(use-package prescient
  :after corfu
  :config
  (defun dima-corfu-prescient-remember (&rest _)
    "Advice for `corfu--insert.'"
    (when (>= corfu--index 0)
      (prescient-remember (nth corfu--index corfu--candidates))))

  (advice-add #'corfu--insert :before #'dima-corfu-prescient-remember)

  (add-to-list 'completion-styles 'prescient)
  (setq corfu-sort-function #'prescient-sort)
  (setq corfu-sort-override-function #'prescient-sort))

(use-package prism
  :disabled t
  :hook ((lisp-mode clojure-mode json-mode) . prism-mode)
  :hook ((lisp-mode python-mode) . prism-whitespace-mode))

;;;_ , projectile

(use-package project
  :bind ( :map project-prefix-map
          ("s" . project-save-some-buffers))
  :preface
  (unless (boundp 'project-switch-commands)
    (defvar project-switch-commands nil))
  :custom
  (project-compilation-buffer-name-function 'project-prefixed-buffer-name)
  (project-switch-commands (list))
  :preface
  (defcustom project-root-markers
    '("Cargo.toml" "compile_commands.json" "compile_flags.txt"
      "project.clj" ".git" "deps.edn" "shadow-cljs.edn")
    "Files or directories that indicate the root of a project."
    :type '(repeat string)
    :group 'project)
  :config
  (add-to-list 'project-switch-commands
               '(project-switch-to-buffer "Switch buffer"))
  (defun project-root-p (path)
    "Check if the current PATH has any of the project root markers."
    (catch 'found
      (dolist (marker project-root-markers)
        (when (file-exists-p (concat path marker))
          (throw 'found marker)))))
  (defun project-find-root (path)
    "Search up the PATH for `project-root-markers'."
    (when-let ((root (locate-dominating-file path #'project-root-p)))
      (cons 'transient (expand-file-name root))))
  (add-to-list 'project-find-functions #'project-find-root)
  (define-advice project-compile (:around (fn) save-project-buffers)
    "Only ask to save project-related buffers."
    (let* ((project-buffers (project-buffers (project-current)))
           (compilation-save-buffers-predicate
            (lambda () (memq (current-buffer) project-buffers))))
      (funcall fn)))
  (define-advice recompile (:around (fn &optional edit-command) save-project-buffers)
    "Only ask to save project-related buffers if inside of a project."
    (if (project-current)
        (let* ((project-buffers (project-buffers (project-current)))
               (compilation-save-buffers-predicate
                (lambda () (memq (current-buffer) project-buffers))))
          (funcall fn edit-command))
      (funcall fn edit-command)))
  (defun project-save-some-buffers (&optional arg)
    "Save some modified file-visiting buffers in the current project.
Optional argument ARG (interactively, prefix argument) non-nil
means save all with no questions."
    (interactive "P")
    (let* ((project-buffers (project-buffers (project-current)))
           (pred (lambda () (memq (current-buffer) project-buffers))))
      (funcall-interactively #'save-some-buffers arg pred))))

(use-package project-x
  :straight (:host github :repo "karthink/project-x")
  :after project
  :config
  (setq project-x-save-interval 600)  ;Save project state every 10 min
  (project-x-mode +1))

(use-package projectile
  :disabled t
  :bind-keymap (("C-c p" . projectile-mode-map))
  :custom
  (projectile-keymap-prefix "C-c p")
  (projectile-require-project-root t)
  (projectile-completion-system 'ivy)
  (projectile-enable-caching t)
  (projectile-project-root-files-functions
   '(;; projectile-root-local
     projectile-root-bottom-up
     projectile-root-top-down
     projectile-root-top-down-recurring)
   "Omit root-local strategy")
  (projectile-create-missing-test-files t)
  :config
  (setq projectile-project-root-files-bottom-up
        (append projectile-project-root-files
                projectile-project-root-files-bottom-up))
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (defun my-projectile-invalidate-cache (&rest _args)
    ;; We ignore the args to `magit-checkout'.
    (projectile-invalidate-cache nil))

  (eval-after-load 'magit-branch
    '(progn
       (advice-add 'magit-checkout
                   :after #'my-projectile-invalidate-cache)
       (advice-add 'magit-branch-and-checkout
                   :after #'my-projectile-invalidate-cache)))
  (projectile-mode +1))

(use-package counsel-projectile
  :disabled t
  :after projectile
  ;; :after persp-projectile
  :custom
  (counsel-projectile-grep-initial-input '(ivy-thing-at-point))
  (projectile-mode-line
   '(:eval (if (file-remote-p default-directory)
               " P[*remote*]"
             (format " P[%s]" (projectile-project-name)))))
  (counsel-projectile-org-capture-templates
   '(("j" "Journal entry" plain
      (file+datetree "${root}/notes.org" "Journal")
      "**** %<%H:%M>\n%?" :clock-in t :clock-resume t)
     ("t" "Todo" entry
      (file+datetree "${root}/notes.org")
      "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
     ("s" "Code Snippet"
      (file+datetree "${root}/notes.org")
      ;; Prompt for tag and language
      "* %?\t%^g\n#+BEGIN_SRC %^{language}\n\n#+END_SRC"
      :clock-in t :clock-resume t)
     ("c" "Capture through org protocol" entry
      (file+headline "${root}/notes.org")
      "* %?%:description\n:PROPERTIES:\n:URL: %:link\n:END:\n\n"
      :clock-in t :clock-resume t)
     ("r" "Respond" entry
      (file "${root}/notes.org")
      "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n"
      :clock-in t :clock-resume t)
     ("n" "Note" entry
      (file+datetree ("${root}/notes.org"))
      "* \n\n"
      :clock-in t :clock-resume t)
     ("j" "Journal" entry
      (file ("${root}/notes.org"))
      "* %?\n%U\n" :clock-in t :clock-resume t)
     ("w" "org-protocol" entry
      (file ("${root}/notes.org"))
      "* TODO Review %c\n%U\n" :clock-in t :clock-resume t)
     ("m" "Meeting" entry
      (file ("${root}/notes.org"))
      "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
     ("p" "Phone call" entry
      (file ("${root}/notes.org"))
      "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
     ("h" "Habit" entry
      (file ("${root}/notes.org"))
      (concat "* NEXT %?\n%U\n%a\nSCHEDULED: "
              " %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n"
              ":PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n"
              ":END:\n"))))
  :config
  (progn
    (counsel-projectile-mode +1)))

(use-package helm-projectile
  :disabled t
  :after projectile
  :config
  (progn
    (helm-projectile-on)))

(use-package projectile-speedbar
  :disabled t ;; causes error on init
  :after projectile)

;;;_ , ps-print

(use-package ps-print
  :defer t
  :custom
  (ps-font-size '(8 . 10))
  (ps-footer-font-size '(12 . 14))
  (ps-header-font-size '(12 . 14))
  (ps-header-title-font-size '(14 . 16))
  (ps-line-number-font-size 10)
  (ps-print-color-p nil)
  :preface
  (defun ps-spool-to-pdf (beg end &rest _ignore)
    (interactive "r")
    (let ((temp-file (concat (make-temp-name "ps2pdf") ".pdf")))
      (call-process-region beg end (executable-find "ps2pdf")
                           nil nil nil "-" temp-file)
      (call-process (executable-find "open") nil nil nil temp-file)))
  :config
  (setq ps-print-region-function 'ps-spool-to-pdf))

(use-package pulsar
  :custom
  (pulsar-pulse-functions
   '(isearch-repeat-forward
     isearch-repeat-backward
     recenter-top-bottom
     move-to-window-line-top-bottom
     reposition-window
     bookmark-jump
     other-window
     delete-window
     delete-other-windows
     forward-page
     backward-page
     scroll-up-command
     scroll-down-command
     windmove-right
     windmove-left
     windmove-up
     windmove-down
     windmove-swap-states-right
     windmove-swap-states-left
     windmove-swap-states-up
     windmove-swap-states-down
     tab-new
     tab-close
     tab-next
     org-next-visible-heading
     org-previous-visible-heading
     org-forward-heading-same-level
     org-backward-heading-same-level
     outline-backward-same-level
     outline-forward-same-level
     outline-next-visible-heading
     outline-previous-visible-heading
     outline-up-heading))

  (pulsar-pulse t)
  (pulsar-delay 0.055)
  (pulsar-iterations 10)
  (pulsar-face 'pulsar-magenta)
  (pulsar-highlight-face 'pulsar-yellow)
  :config
  (pulsar-global-mode +1))


(use-package puni
  :defer t
  :init
  (puni-global-mode)
  :hook (('term-mode-hook
          'lisp-mode-hook
          'clojure-mode-hook
          'clojurescript-mode-hook
          'cider-mode-hook) . #'puni-disable-puni-mode))

;;;_ , puppet-mode

(use-package puppet-mode)

;;;_ , python-mode

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :custom
  (python-fill-docstring-style 'symmetric)
  :config
  (progn
    (defvar python-mode-initialized nil)

    (defun my-python-mode-hook ()
      (setq fill-column 72)
      (unless python-mode-initialized
        (setq python-mode-initialized t)

        (info-lookup-add-help
         :mode 'python-mode
         :regexp "[a-zA-Z_0-9.]+"
         :doc-spec
         '(("(python)Python Module Index")
           ("(python)Index"
            (lambda
              (item)
              (cond
               ((string-match
                 "\\([A-Za-z0-9_]+\\)() (in module \\([A-Za-z0-9_.]+\\))" item)
                (format "%s.%s" (match-string 2 item)
                        (match-string 1 item)))))))))

      (bind-key "C-c C-z" 'python-shell python-mode-map)
      (unbind-key "C-c c" python-mode-map))
    (add-hook 'python-mode-hook 'my-python-mode-hook)))

;;;_ , quickrun

(use-package quickrun
  :defer t
  :bind ("C-c C-M-r" . quickrun))

;;;_ , rainbow-mode

(use-package rainbow-delimiters
  :unless noninteractive
  :hook ((inferior-emacs-lisp-mode
          prog-mode) . #'rainbow-delimiters-mode))

(use-package rainbow-mode
  :commands rainbow-mode)

;;;_ , recentf

(use-package recentf
  :demand t
  :commands (recentf-mode
             recentf-add-file
             recentf-apply-filename-handlers)
  :custom
  (recentf-auto-cleanup 60)
  (recentf-exclude
   '("~\\'" "\\`out\\'" "\\.log\\'" "^/[^/]*:" "\\.el\\.gz\\'"))
  (recentf-max-saved-items 2000)
  (recentf-save-file (user-data "recentf"))
  :preface
  (defun recentf-add-dired-directory ()
    "Add directories visit by dired into recentf."
    (if (and dired-directory
             (file-directory-p dired-directory)
             (not (string= "/" dired-directory)))
        (let ((last-idx (1- (length dired-directory))))
          (recentf-add-file
           (if (= ?/ (aref dired-directory last-idx))
               (substring dired-directory 0 last-idx)
             dired-directory)))))
  :hook (dired-mode . recentf-add-dired-directory)
  :config
  (recentf-mode 1))

(use-package repl-toggle
  :custom
  (rtog/mode-repl-alist
   '((emacs-lisp-mode . ielm)
     (ruby-mode . inf-ruby)
     (js-mode . nodejs-repl)
     (typescript-mode . run-ts)))
  :config
  (repl-toggle-mode))

;;;_ , restart-emacs

(defun mak::restart-emacs-or-release-file ()
  "Optionally restart EMACS."
  (interactive)
  (if server-buffer-clients
      (server-edit)
    (when (y-or-n-p "Restart Emacs? ")
      (restart-emacs '("--debug-init")))))

(use-package restart-emacs
  :disabled t
  ;; BULK-ENSURE :ensure t
  :defer t
  :bind ("C-x C-c" . mak::restart-emacs-or-release-file))

;;;_ , ruby-mode

(use-package ruby-mode
  :defer t
  :mode (("\\.rb\\'" . ruby-mode)
         ("\\.rake$" . ruby-mode)
         ("\\.gemspec$" . ruby-mode)
         ("\\.ru$" . ruby-mode)
         ("Rakefile$" . ruby-mode)
         ("Thorfile$" . ruby-mode)
         ("Gemfile$" . ruby-mode)
         ("Capfile$" . ruby-mode)
         ("Guardfile$" . ruby-mode))
  :interpreter ("ruby" . ruby-mode))

(use-package rbenv
  :after ruby-mode
  :config
  (progn
    (add-hook 'ruby-mode-hook 'rbenv-use-corresponding)
    (setq
     rbenv-modeline-function 'rbenv--modeline-plain
     rbenv-show-active-ruby-in-modeline nil)
    (global-rbenv-mode)))

(use-package ruby-tools
  :after ruby-mode)

(use-package rhtml-mode
  :after ruby-mode
  :mode (("\\.rhtml$" . rhtml-mode)
         ("\\.html\\.erb$" . rhtml-mode)))

(use-package rinari
  :after ruby-mode
  :config
  (global-rinari-mode 1)
  :config (setq ruby-insert-encoding-magic-comment nil))

(use-package rspec-mode
  :after ruby-mode
  :config
  (progn
    (add-hook 'ruby-mode-hook 'rspec-mode)
    (setq rspec-use-rake-when-possible nil)
    (defadvice rspec-compile (around rspec-compile-around activate)
      "Use BASH shell for running the specs because of ZSH issues."
      (let ((shell-file-name "/bin/bash"))
        ad-do-it))))

(use-package robe
  :after ruby-mode
  :config
  (progn
    (add-hook 'ruby-mode-hook 'robe-mode)
    (with-eval-after-load 'company
      (add-to-list 'company-backends 'company-robe))))

(use-package enh-ruby-mode
  :after ruby-mode
  :config
  (progn
    (add-hook 'enh-ruby-mode-hook 'robe-mode)
    (add-hook 'enh-ruby-mode-hook 'yard-mode)))

(setenv "JRUBY_OPTS" "--2.0")

(use-package yard-mode
  :config
  (progn
    (add-hook 'ruby-mode-hook 'yard-mode)))

;;;_ , ryo-modal

(use-package ryo-modal
  :commands ryo-modal-mode
  :bind ("C-c SPC" . ryo-modal-mode)
  :init
  (add-hook 'ryo-modal-mode-hook
            (lambda ()
              (if ryo-modal-mode
                  (selected-minor-mode 1)
                (selected-minor-mode -1))))
  :config
  (ryo-modal-keys
   ("q" ryo-modal-mode)
   ("0" "M-0")
   ("1" "M-1")
   ("2" "M-2")
   ("3" "M-3")
   ("4" "M-4")
   ("5" "M-5")
   ("6" "M-6")
   ("7" "M-7")
   ("8" "M-8")
   ("9" "M-9")
   ("h" backward-char)
   ("j" next-line)
   ("k" previous-line)
   ("l" forward-char)))

;;;_ , savehist

(use-package savehist
  :unless noninteractive
  :custom
  (savehist-additional-variables
   '(file-name-history
     kmacro-ring
     compile-history
     compile-command))
  (savehist-autosave-interval 60)
  (savehist-file (user-data "history"))
  (savehist-ignored-variables
   '(load-history
     flyspell-auto-correct-ring
     org-roam-node-history
     magit-revision-history
     org-read-date-history
     query-replace-history
     yes-or-no-p-history
     kill-ring))
  (savehist-mode t)
  :config
  (savehist-mode +1))

(use-package saveplace
  :config
  (save-place-mode +1))

;;;_ , sed-mode

(use-package sed-mode)

;;;_ , select-themes

(use-package select-themes)

;;;_ , selected

(use-package serenade-mode
  :disabled t
  :straight (serenade-mode :type git :host github
                           :repo "justin-roche/serenade-mode"))

(use-package selected
  :diminish selected-minor-mode
  :bind (:map selected-keymap
              ("[" . align-code)
              ("f" . fill-region)
              ("U" . unfill-region)
              ("d" . downcase-region)
              ("u" . upcase-region)
              ("r" . reverse-region)
              ("s" . sort-lines))
  :config
  (selected-global-mode +1))

;;;_ , sh-script

(use-package sh-script
  :defer t
  :config
  (progn
    (defvar sh-script-initialized nil)
    (defun initialize-sh-script ()
      (unless sh-script-initialized
        (setq sh-script-initialized t)
        (info-lookup-add-help :mode 'shell-script-mode
                              :regexp ".*"
                              :doc-spec
                              '(("(bash)Index")))))

    (add-hook 'shell-mode-hook 'initialize-sh-script)))

;;;_ , sh-toggle

(use-package sh-toggle
  :disabled t
  :bind ("C-x C-z" . shell-toggle))

(use-package shx
  :hook (shell-mode . shx-mode))

;;;_ , shackle

(use-package shackle
  :disabled t
  ;; BULK-ENSURE :ensure t
  :diminish shackle-mode
  :init
  (defun custom/shackle--smart-split-dir ()
    (if (>= (window-pixel-height)
            (window-pixel-width))
        'below
      'right))

  (defun shackle-split (buffer alist plist)
    (let
        ((frame (shackle--splittable-frame))
         (window (if (eq (custom/shackle--smart-split-dir) 'below)
                     (split-window-below)
                   (split-window-right))))
      (prog1
          (window--display-buffer buffer window 'window alist display-buffer-mark-dedicated)
        (when window
          (setq shackle-last-window window
                shackle-last-buffer buffer))
        (unless (cdr (assq 'inhibit-switch-frame alist))
          (window--maybe-raise-frame frame)))))
  :custom
  (shackle-rules
   '(((svg-2048-mode
       circe-query-mode)       :same t)
     ((flycheck-error-list-mode
       compilation-mode)       :select nil :custom shackle-split :size 0.25)
     ((messages-buffer-mode
       calendar-mode)          :select t   :custom shackle-split :size 0.25)
     ((ert-results-mode
       racer-help-mode
       help-mode
       helpful-mode)           :select t   :custom shackle-split :size 0.5)
     ("*Org Select*"           :select t   :align below :size 0.33)
     ("*Org Note*"             :select t   :align below :size 0.33)
     ("*Org Links*"            :select t   :align below :size 0.2)
     (" *Org todo*"            :select t   :align below :size 0.2)
     ("*Man.*"                 :select t   :align below :size 0.5  :regexp t)
     ("*helm.*"                :select t   :align below :size 0.33 :regexp t)
     ("*Org Src.*"             :select t   :align right :size 0.5  :regexp t)
     ("*Help*"                   :align t :select t)
     ("#.*@.*"                   :regexp t :same t)
     ("[^#].*@.*"                :regexp t :same t)
     (occur-mode                 :select nil :align t)
     ("*eshell*"                 :select t :other t)
     ("*Messages*"               :select nil :inhibit-window-quit t :other t)
     (magit-status-mode          :select t :same t)
     (magit-log-mode             :select t :same t)
     ("\\`\\*helm.*?\\*\\'"      :regexp t :align t)
     ((compilation-mode
       "\\`\\*firestarter\\*\\'"
       "\\`\\*magit-diff: .*?\\'") :regexp t :noselect t)
     ("*Calendar*"                 :select t :size 0.3 :align below)
     ("*info*"                     :select t :inhibit-window-quit t :same t)
     ("\\`\\*cider-repl .*"        :regexp t :align t :size 0.2)
     ((inferior-scheme-mode
       "*shell*"
       "*eshell*")                 :popup t)))
  (shackle-default-rule '(:select t))
  (shackle-default-size 0.4)
  (shackle-inhibit-window-quit-on-same-windows t)
  :config
  (add-hook 'after-init-hook '(lambda () (shackle-mode +1)) t))

(use-package shift-number
  :bind (("C-c +" . shift-number-up)
         ("C-c -" . shift-number-down)))

;;;_ , slack

(use-package slack
  :disabled t
  ;; :defer t
  ;; BULK-ENSURE :ensure t
  :after alert-play
  ;; :commands (slack-start)
  :preface
  ;; prevent free variable warnings
  (defvar url-http-method "")
  (defvar url-http-data "")
  (defvar url-http-extra-headers "")
  (defvar url-callback-function (function ()))
  (defvar url-callback-arguments (list))
  (defvar oauth--token-data "")
  ;; :custom-face (slack-message-output-text ((t (:font "Roboto Condensed"
  ;;                                                    :width 'condensed))))
  :custom
  (lui-fill-column 140)
  (lui-time-stamp-format "[%H:%M]")
  (slack-prefer-current-team t)
  (slack-buffer-function #'switch-to-buffer)
  (slack-buffer-create-on-notify t)
  (slack-enable-emoji t)
  (slack-request-timeout 600)
  :secret (slack-room-subscription
           slack-client-id
           slack-client-secret
           slack-token
           slack-user-name)
  :config
  (set-face-attribute 'slack-message-output-text
                      nil
                      :font "Roboto Condensed"
                      :width 'condensed)
  (define-key slack-mode-map "@"
    (defun my-slack-message-embed-mention ()
      (interactive)
      (call-interactively #'slack-message-embed-mention)
      (insert " ")))

  (when-let* ((dir (ensure-user-dir "slack"))
              (file (expand-file-name "teams.el" dir))
              (_ (file-exists-p file)))
    (load-file file))

  (defun my-slack-message-notifier (message room team)
    "My custom notification for slack given MESSAGE, ROOM and TEAM."
    (when (and (not (slack-message-minep message team))
               (or (slack-im-p room)
                   (and (slack-group-p room)
                        (slack-mpim-p room))
                   (slack-room-subscribedp room team)
                   (string-match
                    (format "@%s" (plist-get (oref team self) :name))
                    (or (slack-message-body message team) ""))))
      (let* (
             (team-name (oref team name))
             (room-name (slack-room-name room team))
             (text (slack-message-to-alert message team))
             (user-name (slack-message-sender-name message team)))

        (message "alerting for team: %s room: %s user: %s \"%s\" "
                 team-name room-name user-name text)
        (alert text
               :title room-name
               :category "slack"))))

  (alert-add-rule :category "slack"
                  :style 'play
                  :continue t)

  (add-to-list 'alert-play-category-map
               '("slack"
                 ((:title
                   ((::any "/home/emacs/imsounds/slack/knock_brush.mp3"))))))

  (setq slack-message-custom-notifier #'my-slack-message-notifier
        slack-message-custom-delete-notifier #'my-slack-message-notifier))

(use-package spell-fu
  :disabled t
  :unless noninteractive
  :config
  (global-spell-fu-mode +1))

(let ((spotify-credentials
       (expand-file-name "credentials.el"
                         (locate-user-emacs-file "spotify/"))))
  (use-package spotify
    :when (file-exists-p spotify-credentials)
    :config
    (setq spotify-transport 'connect)
    (with-temp-buffer
      (insert-file-contents
       spotify-credentials)
      (eval-buffer))
    ))

(use-package ssh-agency)

(use-package tracking
  :defer t
  :config
  (define-key tracking-mode-map [(control ?c) space] #'tracking-next-buffer))

(use-package typescript-mode
  :custom
  (typescript-indent-level 2))

(use-package tide
  :diminish tide-mode
  :after typescript-mode
  :config
  (add-hook 'typescript-mode-hook #'tide-setup))

;;;_ , slime

(use-package slime
  :commands slime
  :init
  ;; (unless (memq major-mode
  ;;               '(emacs-lisp-mode inferior-emacs-lisp-mode ielm-mode))
  ;;   ("M-q" . slime-reindent-defun)
  ;;   ("M-l" . slime-selector))

  (setq inferior-lisp-program "sbcl"
        slime-contribs '(slime-fancy))
  :custom
  (slime-kill-without-query-p t)
  (slime-repl-history-file
   (expand-file-name "slime-history.eld"
                     (ensure-user-dir "slime/")) t)
  (slime-startup-animation nil))

;;;_ , smart-backspace

(use-package smart-backspace
  :disabled t
  :hook (prog-mode . #'(lambda ()
                         (local-set-key "<backspace>" 'smart-backspace))))

;;;_ , smart-compile

(use-package smart-compile
  :disabled t
  :defer t
  :commands smart-compile
  :bind (("C-c b" . smart-compile)
         ("A-n"   . next-error)
         ("A-p"   . previous-error))
  :init
  (progn
    (defun show-compilation ()
      (interactive)
      (let ((compile-buf
             (catch 'found
               (dolist (buf (buffer-list))
                 (if (string-match "\\*compilation\\*" (buffer-name buf))
                     (throw 'found buf))))))
        (if compile-buf
            (switch-to-buffer-other-window compile-buf)
          (call-interactively 'compile))))

    (bind-key "M-O" 'show-compilation)))

;;;_ , smart-tabs-mode

(use-package smart-tabs-mode
  :defer t
  :commands smart-tabs-mode)

;;;_ , solaire-mode

(use-package solaire-mode
  :config
  (solaire-mode)
  (add-hook 'after-change-major-mode-hook 'turn-on-solaire-mode))

;;;_ , solarized-theme

;;;_ , sparql-mode

(use-package sparql-mode
  :mode ("\\.sparql\\'" . sparql-mode))

;;;_ , speech-tagger

;; (use-package speech-tagger
;;   :unless noninteractive
;;   )

;;;_ , sqlup

(use-package sqlite3
  :defer t)

(use-package sqlup-mode
  :defer t
  :unless noninteractive
  :config
  (progn
    (add-hook 'sql-mode-hook 'sqlup-mode)
    ;; Capitalize keywords in an interactive session (e.g. psql)
    (add-hook 'sql-interactive-mode-hook 'sqlup-mode)))

;;;_ , sr-speedbar

(use-package sr-speedbar
  :unless noninteractive)

(use-package string-inflection
  :bind ("C-c `" . string-inflection-all-cycle))

;;;_ , stopwatch

(use-package stopwatch
  :straight (:host github :repo "blue0513/stopwatch")
  :bind ("<f8>" . stopwatch))

(use-package string-inflection
  :bind ("C-c `" . string-inflection-toggle))

;;;_ , symbol-overlay

(use-package symbol-overlay
  :diminish
  :custom-face (symbol-overlay-default-face ((t (:inherit (region bold)))))
  :bind (("M-g i" . symbol-overlay-put)
         ("M-g s n" . symbol-overlay-jump-next)
         ("M-g s p" . symbol-overlay-jump-prev)
         ("M-g s N" . symbol-overlay-switch-forward)
         ("M-g s P" . symbol-overlay-switch-backward)
         ("M-g s C" . symbol-overlay-remove-all)
         ([M-f3] . symbol-overlay-remove-all))
  :hook ((prog-mode . symbol-overlay-mode)
         (iedit-mode . turn-off-symbol-overlay)
         (iedit-mode-end . turn-on-symbol-overlay))
  :init (setq symbol-overlay-idle-time 0.1)
  (with-eval-after-load 'all-the-icons
    (setq symbol-overlay-faces
          '((:inherit (all-the-icons-blue bold) :inverse-video t)
            (:inherit (all-the-icons-pink bold) :inverse-video t)
            (:inherit (all-the-icons-yellow bold) :inverse-video t)
            (:inherit (all-the-icons-maroon bold) :inverse-video t)
            (:inherit (all-the-icons-red bold) :inverse-video t)
            (:inherit (all-the-icons-orange bold) :inverse-video t)
            (:inherit (all-the-icons-green bold) :inverse-video t)
            (:inherit (all-the-icons-cyan bold) :inverse-video t))))
  :config
  ;; Disable symbol highlighting while selecting
  (defun turn-off-symbol-overlay (&rest _)
    "Turn off symbol highlighting."
    (interactive)
    (symbol-overlay-mode -1))
  (advice-add #'set-mark :after #'turn-off-symbol-overlay)

  (defun turn-on-symbol-overlay (&rest _)
    "Turn on symbol highlighting."
    (interactive)
    (when (derived-mode-p 'prog-mode)
      (symbol-overlay-mode +1)))
  (advice-add #'deactivate-mark :after #'turn-on-symbol-overlay))


;; (use-package symbol-overlay
;;   :init
;;   (defun symbol-overlay ())
;;   :custom-face
;;   (symbol-overlay-face-1
;;    ((t (:box (:line-width -1 :color "dodger blue")))))
;;   (symbol-overlay-face-2
;;    ((t (:box (:line-width -1 :color "hot pink")))))
;;   (symbol-overlay-face-3
;;    ((t (:box (:line-width -1 :color "yellow")))))
;;   (symbol-overlay-face-4
;;    ((t (:box (:line-width -1 :color "orchid")))))
;;   (symbol-overlay-face-5
;;    ((t (:box (:line-width -1 :color "red")))))
;;   (symbol-overlay-face-6
;;    ((t (:box (:line-width -1 :color "salmon")))))
;;   (symbol-overlay-face-7
;;    ((t (:box (:line-width -1 :color "spring green")))))
;;   (symbol-overlay-face-8
;;    ((t (:box (:line-width -1 :color "turquoise")))))
;;   :bind (:map symbol-overlay-mode-map
;;               ("M-i" . symbol-overlay-put)
;;               ("M-n" . symbol-overlay-jump-next)
;;               ("M-p" . symbol-overlay-jump-prev))
;;   :hook (prog-mode . #'symbol-overlay-mode)
;;   :hook (html-mode . #'symbol-overlay-mode)
;;   :hook (prog-mode . #'symbol-overlay-mode)
;;   :diminish symbol-overlay-mode)

;;;_ , systemd

(use-package systemd
  :mode ("\\.automount\\'\\|\\.busname\\'\\|\\.mount\\'\\|\\.service\\'\\|\\.slice\\'\\|\\.socket\\'\\|\\.target\\'\\|\\.timer\\'\\|\\.link\\'\\|\\.netdev\\'\\|\\.network\\'\\|\\.override\\.conf.*\\'" . systemd-mode))

;;;_ , tagedit

(use-package tagedit
  :config
  (tagedit-add-paredit-like-keybindings)
  :hook ('html-mode . (lambda () (tagedit-mode 1))))

(use-package tempel
  ;; Require trigger prefix before template name when completing.
  ;; :custom
  ;; (tempel-trigger-prefix "<")

  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert))

  :init
  ;; (defvar my-global-templates
  ;;   '((example "Global example template"))
  ;;   "My global templates.")
  ;; (defvar-local my-local-templates nil
  ;;   "Buffer-local templates.")
  ;; (add-to-list 'tempel-template-sources 'my-global-templates)
  ;; (add-to-list 'tempel-template-sources 'my-local-templates)

  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)

  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (global-tempel-abbrev-mode)
  )

(use-package terraform-mode)

(use-package typescript-mode
  :mode "\.ts\\'")

;;;_ , texinfo

(use-package texinfo
  :defines texinfo-section-list
  :mode ("\\.texi\\'" . texinfo-mode)
  :config
  (progn
    (defun my-texinfo-mode-hook ()
      (dolist (mapping '((?b . "emph")
                         (?c . "code")
                         (?s . "samp")
                         (?d . "dfn")
                         (?o . "option")
                         (?x . "pxref")))
        (local-set-key (vector (list 'alt (car mapping)))
                       `(lambda () (interactive)
                          (TeX-insert-macro ,(cdr mapping))))))

    (add-hook 'texinfo-mode-hook 'my-texinfo-mode-hook)

    (defun texinfo-outline-level ()
      ;; Calculate level of current texinfo outline heading.
      (require 'texinfo)
      (save-excursion
        (if (bobp)
            0
          (forward-char 1)
          (let* ((word (buffer-substring-no-properties
                        (point) (progn (forward-word 1) (point))))
                 (entry (assoc word texinfo-section-list)))
            (if entry
                (nth 1 entry)
              5)))))))

(use-package text-mode
  :straight nil
  :defer t
  :preface
  (eval-when-compile
    (require 'diminish))
  :hook
  (text-mode . turn-on-auto-fill)
  (text-mode . (lambda ()
                 (ignore-errors
                   (diminish 'auto-fill-function)))))

;;;_ , todochiku

;; (use-package todochiku)

;;;_ , tramp

(use-package tramp
  :defer t
  :custom
  (tramp-default-method "ssh")
  (tramp-auto-save-directory "~/.cache/emacs/backups")
  :config
  ;; jww (2018-02-20): Without this change, tramp ends up sending hundreds of
  ;; shell commands to the remote side to ask what the temporary directory is.
  (put 'temporary-file-directory 'standard-value '("/tmp"))

  ;; Setting this with `:custom' does not take effect.
  (setq tramp-persistency-file-name (user-data "tramp")))

(use-package transient
  :defer t
  :custom
  (transient-history-file (user-data "transient/history.el"))
  (transient-values-file (user-data "transient/values.el")))

(use-package transpose-mark
  :commands (transpose-mark
             transpose-mark-line
             transpose-mark-region))

(use-package undo-propose
  :commands undo-propose)

;;;_ , uniquify

(use-package uniquify
  :straight nil
  :custom
  (uniquify-buffer-name-style 'forward)
  (uniquify-separator "/")
  ;; rename after killing uniquified
  (uniquify-after-kill-buffer-p t)
  ;; don't muck with special buffers
  (uniquify-ignore-buffers-re "^\\*"))

(use-package uuidgen
  :commands (insert-uuid-cid))

;;;_ , all-the-icons

(use-package all-the-icons)

(use-package all-the-icons-completion
  :config
  :after all-the-icons
  (all-the-icons-completion-mode))

(use-package all-the-icons-dired
  :hook (dired-mode . (lambda () (all-the-icons-dired-mode +1))))

(use-package vc-backup)

(use-package vdiff
  :commands (vdiff-files
             vdiff-files3
             vdiff-buffers
             vdiff-buffers3))

;;;_ , volatile highlights - temporarily highlight changes from pasting etc

(use-package volatile-highlights
  :disabled t                           ;in favor of goggles
  :diminish " 🌋"
  :config
  (volatile-highlights-mode t))

(use-package verb)

(use-package vertico
  :init
  (vertico-mode)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)

  :bind (:map vertico-map
              ("M-RET" . vertico-exit-input))
  :config
  (add-to-list 'completion-styles 'substring)
  (add-to-list 'completion-styles 'flex)

  (setq read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t
        completion-ignore-case t))

(use-package vertico-directory
  :after vertico
  :straight nil
  :preface
  (load-file (expand-file-name
              "straight/repos/vertico/extensions/vertico-directory.el"
              straight-base-dir ))
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; (use-package vertico-quick
;;   :after vertico
;;   :straight nil
;;   :preface
;;   (load-file (expand-file-name
;;               "straight/repos/vertico/extensions/vertico-quick.el"
;;               straight-base-dir ))
;;   :bind (
;;          :map vertico-map
;;          ("M-q" . vertico-quick-insert)
;;          ("C-q" . vertico-quick-exit))
;;   :init
;;   (progn
;;     (setq vertico-quick1 "arstdhn")
;;     (setq vertico-quick2 "oie")))

;; (use-package vertico-multiform
;;   :after vertico
;;   :straight nil
;;   :preface
;;   (load-file (expand-file-name
;;               "straight/repos/vertico/extensions/vertico-multiform.el"
;;               straight-base-dir ))
;;   :config
;;   (vertico-multiform-mode +1)

;;   (setq vertico-multiform-commands
;;         '(;; show grep results in a dedicated buffer:
;;           (consult-ripgrep buffer))))

(use-package visual-fill-column
  :commands visual-fill-column-mode)

(use-package virtual-auto-fill
  :commands virtual-auto-fill-mode)

(use-package virtual-auto-fill
  :commands virtual-auto-fill-mode)

(use-package vterm
  :hook (vterm-mode . (lambda () (disable-mouse-mode -1))))

;;;_ , w3m

(use-package w3m
  :commands (w3m-browse-url w3m-find-file)
  :custom
  (w3m-cookie-accept-bad-cookies 'ask)
  (w3m-default-display-inline-images t)
  (w3m-fill-column 100)
  (w3m-use-cookies t))

(use-package web-mode
  :commands web-mode)

(use-package wgrep
  :custom
  (wgrep-auto-save-buffer t)
  (wgrep-enable-key "��"))

;;;_ , which-key

(use-package which-func
  :straight nil
  :hook (prog-mode . which-function-mode))

(use-package which-key
  :diminish which-key-mode
  :unless noninteractive
  :config
  (which-key-mode))

;;;_ , whitespace

(use-package whitespace
  :diminish (global-whitespace-mode
             whitespace-mode
             whitespace-newline-mode)
  :commands (whitespace-buffer
             whitespace-cleanup
             whitespace-mode)
  :hook (prog-mode . (lambda () (whitespace-mode +1)) )
  :init
  (progn
    (defun normalize-file ()
      (interactive)
      (save-excursion
        (goto-char (point-min))
        (whitespace-cleanup)
        (delete-trailing-whitespace)
        (goto-char (point-max))
        (delete-blank-lines)
        (set-buffer-file-coding-system 'unix)
        (goto-char (point-min))
        (while (re-search-forward "\r$" nil t)
          (replace-match ""))
        (set-buffer-file-coding-system 'utf-8)
        (let ((require-final-newline t))
          (save-buffer))))

    (defun maybe-turn-on-whitespace ()
      "Depending on the file, maybe clean up whitespace."
      (let ((file (expand-file-name ".clean"))
            parent-dir)
        (while (and (not (file-exists-p file))
                    (progn
                      (setq parent-dir
                            (file-name-directory
                             (directory-file-name
                              (file-name-directory file))))
                      ;; Give up if we are already at the root dir.
                      (not (string= (file-name-directory file)
                                    parent-dir))))
          ;; Move up to the parent dir and try again.
          (setq file (expand-file-name ".clean" parent-dir)))
        ;; If we found a change log in a parent, use that.
        (when (and (file-exists-p file)
                   (not (file-exists-p ".noclean"))
                   (not (and buffer-file-name
                             (string-match "\\.texi\\'" buffer-file-name))))
          (add-hook 'write-contents-hooks
                    #'(lambda ()
                        (ignore (whitespace-cleanup))) nil t)
          (whitespace-cleanup))))

    (add-hook 'find-file-hooks 'maybe-turn-on-whitespace t))
  :config
  (progn
    (setq whitespace-style '(face trailing space-before-tab empty))
    (remove-hook 'find-file-hooks 'whitespace-buffer)
    (remove-hook 'kill-buffer-hook 'whitespace-buffer)))

(use-package whitespace-cleanup-mode
  :demand t
  :diminish
  :commands whitespace-cleanup-mode
  :config
  (global-whitespace-cleanup-mode 1))

;;;_ , window-purpose

(use-package window-purpose
  :init

  (defun turn-on-purpose-mode ()
    (interactive)
    (purpose-mode +1)
    (purpose-x-popwin-setup)
    (purpose-x-kill-setup)
    (when (file-exists-p purpose-default-layout-file)
      (purpose-load-window-layout-file))
    (select-window (get-largest-window)))

  :custom
  (purpose-default-layout-file (user-file-path "window-purpose" "layout.el"))
  :config
  (require 'window-purpose-x)

  (purpose-add-user-purposes
   :modes '((message-mode . edit)
            (ag-mode      . search)
            (rg-mode      . search)))  (purpose-x-magit-single-on))

;; :hook (after-init . #'turn-on-purpose-mode)

;;;_ , winner

(use-package winner
  :unless noninteractive
  :demand t
  :bind (("M-N" . winner-redo)
         ("M-P" . winner-undo))
  :config
  (winner-mode 1))


;;;_ , write-room

(defun write-room ()
  "Make a frame without any bling."
  (interactive)
  ;; to restore:
  ;; (setq mode-line-format (default-value 'mode-line-format))
  (let ((frame (make-frame
                '((minibuffer . nil)
                  (vertical-scroll-bars . nil)
                  (left-fringe . 0); no fringe
                  (right-fringe . 0)
                  (background-mode . dark)
                  (background-color . "cornsilk")
                  (foreground-color . "black")
                  (cursor-color . "green")
                  (border-width . 0)
                  (border-color . "black"); should be unnecessary
                  (internal-border-width . 64); whitespace!
                  (cursor-type . box)
                  (menu-bar-lines . 0)
                  (tool-bar-lines . 0)
                  (fullscreen . fullboth)  ; this should work
                  (unsplittable . t)))))
    (select-frame frame)
    (find-file "~/Documents/Notes.txt")
    (setq mode-line-format nil
          fill-column 65)
    (set-window-margins (selected-window) 50 50)))

(use-package wdired
  :straight nil
  :config (setq wdired-allow-to-change-permissions t))

(use-package wgrep)

(use-package xray
  :bind (("C-h x b" . xray-buffer)
         ("C-h x f" . xray-faces)
         ("C-h x F" . xray-features)
         ("C-h x R" . xray-frame)
         ("C-h x h" . xray-hooks)
         ("C-h x m" . xray-marker)
         ("C-h x o" . xray-overlay)
         ("C-h x p" . xray-position)
         ("C-h x S" . xray-screen)
         ("C-h x s" . xray-symbol)
         ("C-h x w" . xray-window)))

;;;_ , yasnippet

(use-package yasnippet
  :demand t
  :diminish yas-minor-mode
  :commands yas-minor-mode-on
  :bind (("C-c y d" . yas-load-directory)
         ("C-c y i" . yas-insert-snippet)
         ("C-c y f" . yas-visit-snippet-file)
         ("C-c y n" . yas-new-snippet)
         ("C-c y t" . yas-tryout-snippet)
         ("C-c y l" . yas-describe-tables)
         ("C-c y g" . yas-global-mode)
         ("C-c y m" . yas-minor-mode)
         ("C-c y r" . yas-reload-all)
         ("C-c y x" . yas-expand)
         :map yas-keymap
         ("C-i" . yas-next-field-or-maybe-expand))
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :hook (prog-mode . yas-minor-mode-on)
  :custom
  (yas-prompt-functions '(yas-completing-prompt yas-no-prompt))
  (yas-snippet-dirs (list (emacs-path "snippets")))
  (yas-triggers-in-field t)
  (yas-wrap-around-region t)
  :custom-face
  (yas-field-highlight-face ((t (:background "#e4edfc"))))
  :config
  (yas-load-directory (emacs-path "snippets")))

(use-package yasnippet-snippets
  :after yasnippet
  )

(use-package yasnippet-org
  :straight nil
  :no-require t
  :after org
  :hook (org-mode . yas-minor-mode-on))

(use-package consult-yasnippet
  :after (consult yasnippet))


;;;_ , yatemplate

(use-package yatemplate
  :disabled t
  :unless noninteractive
  :after yasnippet
  :custom
  (yatemplate-dir
   (ensure-directory (expand-file-name "autoinsert" common-emacs-directory)))
  :config
  (yatemplate-fill-alist))

;;;_ , yaml-mode

(use-package yaml-mode
  :mode (("\\.yml\\'" . yaml-mode)
         ("\\.yaml$" . yaml-mode)))

;;;_ , yankpad

(use-package yankpad
  :disabled t
  :after company
  :init
  (setq yankpad-file "/var/sharedelpa/org/yankpad.org")
  (when (not (file-exists-p yankpad-file))
    (shell-command (concat "touch " yankpad-file)))
  :config
  (bind-key "C-c C-y" 'yankpad-map)
  (bind-key "<f12>" 'yankpad-expand)
  ;; If you want to complete snippets using company-mode
  (add-to-list 'company-backends #'company-yankpad))

(use-package znc
  :disabled t
  ;; :secret znc-servers
  :custom
  (znc-servers
   `(("audio.local" .
      (7797 t ((freenode . ("ewatcher/freenode" ""))))))))

;;;_. Post initialization

(report-time-since-load)

;;   mode: emacs-lisp
;; Local Variables:
;; byte-compile-warnings: (not docstrings lexical noruntime)

;; ; LocalWords:  CApitals
;;; init.el ends here
