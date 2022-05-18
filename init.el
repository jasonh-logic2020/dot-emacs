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

(setq custom-file (convert-standard-filename
                   (expand-file-name "settings.el" user-emacs-directory))
      dot-org (convert-standard-filename
               (expand-file-name "dot-org.el" user-emacs-directory))
      org-settings (convert-standard-filename
                    (expand-file-name "org-settings.el" user-emacs-directory)))

(load custom-file)

;; (when (window-system)
;;   (set-face-attribute 'default nil
;;                       :height 70
;;                       :width 'extra-condensed
;;                       :family "DejaVu Sans Mono"))

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
    (require 'auth-source-pass)
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

(setq-default
 line-spacing                   1       ; prevent :box redraws
 indent-tabs-mode             nil       ; Use spaces for indentation
 major-mode             'org-mode) ; Org-mode as default mode

(setq
 echo-keystrokes              0.5  ; minibuffer echo delay (default 1 sec)
 auth-sources '("~/.authinfo.gpg") ; set auth store
 initial-major-mode     'org-mode  ; orgmode please
 scroll-step                    1  ; How many lines to scroll at once
 scroll-conservatively      10000  ; Max lines to scroll to recenter point
 scroll-preserve-screen-position t ; Max lines to scroll to recenter point
 auto-window-vscroll            t  ; Adjust scroll for tall glyphs
 epa-pinentry-mode      'loopback  ; ask in emacs
 auto-revert-verbose          nil  ; Be quiet about reverts
 disabled-command-function    nil  ; Enable disabled commands
 display-time-24hr-format       t  ; 24 hour time format
 eshell-hist-ignoredups         t  ; Ignore duplicate history
 eshell-history-size         1000  ; Lengthen Eshell history
 inhibit-startup-screen         t  ; No startup screen
 inhibit-startup-message        t  ; No startup message
 password-cache-expiry        nil  ; Cache TRAMP passwords forever
 sentence-end-double-space    nil
 save-place-file "~/.emacs.d/saved-point-places"
 delete-old-versions            t  ; Delete without asking
 kept-new-versions              6  ; Number of versions to keep
 kept-old-versions              2  ; Number of old versions
 version-control                t  ; Keep versions of every file
 confirm-kill-processes         nil ;kill processes without asking
 show-paren-delay               0) ; Don't delay the paren update

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
      (user-error "No typo at or before point"))))

(setq save-abbrevs 'silently)
(setq-default abbrev-mode t))

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
  :hook ((prog-mode
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

(autoload 'org-cycle "org" nil t)
(autoload 'hippie-expand "hippie-exp" nil t)
(autoload 'indent-according-to-mode "indent" nil t)

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

(eval-when-compile
  (defvar emacs-min-top)
  (defvar emacs-min-left)
  (defvar emacs-min-height)
  (defvar emacs-min-width))

(defun emacs-min ()
  "Switch to minimal window size."
  (interactive)
  (set-frame-parameter (selected-frame) 'fullscreen nil)
  (set-frame-parameter (selected-frame) 'vertical-scroll-bars nil)
  (set-frame-parameter (selected-frame) 'horizontal-scroll-bars nil)
  (set-frame-parameter (selected-frame) 'top emacs-min-top)
  (set-frame-parameter (selected-frame) 'left emacs-min-left)
  (set-frame-parameter (selected-frame) 'height emacs-min-height)
  (set-frame-parameter (selected-frame) 'width emacs-min-width))

;; (if window-system
;;     (add-hook 'after-init-hook 'emacs-min))

(defun emacs-max ()
  "Switch to maximized window size."
  (interactive)
  (if t
      (progn
        (set-frame-parameter (selected-frame) 'fullscreen 'fullboth)
        (set-frame-parameter (selected-frame) 'vertical-scroll-bars nil)
        (set-frame-parameter (selected-frame) 'horizontal-scroll-bars nil))
    (set-frame-parameter (selected-frame) 'top 26)
    (set-frame-parameter (selected-frame) 'left 2)
    (set-frame-parameter (selected-frame) 'width
                         (floor (/ (float (x-display-pixel-width)) 9.15)))
    (if (= 1050 (x-display-pixel-height))
        (set-frame-parameter (selected-frame) 'height
                             (if (>= emacs-major-version 24)
                                 66
                               55))
      (set-frame-parameter (selected-frame) 'height
                           (if (>= emacs-major-version 24)
                               75
                             64)))))

(defun emacs-toggle-size ()
  "Toggle between min and max widow sizes."
  (interactive)
  (if (> (cdr (assq 'width (frame-parameters))) 100)
      (emacs-min)
    (emacs-max)))

(bind-key "C-c m" 'emacs-toggle-size)

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

(bind-key "C-c n" 'insert-user-timestamp)
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
(bind-key "C-c v" 'ffap)

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

(unless (and noninteractive (not window-system))
  (eval-and-compile
    (defun init-windows ()
      "Default window layout."
      (interactive)
      (if (window-full-width-p)
          (split-window-horizontally))
      (other-window 1))
    (add-hook 'after-init-hook  #'init-windows)))

(unless noninteractive
(eval-and-compile
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

  (bind-key "C-c ;" 'endless/comment-line-or-region prog-mode-map)))

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
  :defer 5
  :commands abbrev-mode
  :diminish abbrev-mode
  :preface
  (defun define-abbrev-sedlike (text)
    "Search TEXT for sedlike s/./,/ and add abbrev to mode table."
    (when (string-match "^[Ss]/\\([^/]*\\)/\\([^/]*\\)/" text)
      ;; (message "Added abbrev \"%s\"->\"%s\" "
      ;;          (match-string 1 text)
      ;;          (match-string 2 text))
      (define-abbrev local-abbrev-table
        (match-string 1 text)
        (match-string 2 text))))
  :init
  (defun abbrev ())
  :hook (text-mode . #'abbrev-mode)
  :hook (prog-mode . #'abbrev-mode)
  :hook (erc-mode . #'abbrev-mode)
  :hook (LaTeX-mode . #'abbrev-mode)
  :config
  (progn
    (if (file-exists-p abbrev-file-name)
        (quietly-read-abbrev-file))))

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
  :bind (("C-)" . ace-mc-add-multiple-cursors)
         ("C-M-)" . ace-mc-add-single-cursor)))

;;;_ , ace-window

(use-package ace-window
  :chords ("jw" . ace-jump-char-mode)
  :bind ("M-i" . ace-window))

;;;_ , aggressive-indent

(use-package aggressive-indent
  :config
  (aggressive-indent-global-mode 1))

;;;_ , airline-themes

(use-package airline-themes
  :disabled t
  :init
  (progn
    (require 'airline-themes)
    (load-theme 'airline-doom-one t))
  :config
  (progn
    (set-face-attribute 'mode-line          nil :font "Fira Mono")
    (set-face-attribute 'mode-line-inactive nil :font "Fira Mono")
    (setq powerline-utf-8-separator-left        #xe0b0
          powerline-utf-8-separator-right       #xe0b2
          airline-utf-glyph-separator-left      #xe0b0
          airline-utf-glyph-separator-right     #xe0b2
          airline-utf-glyph-subseparator-left   #xe0b1
          airline-utf-glyph-subseparator-right  #xe0b3
          airline-utf-glyph-branch              #xe0a0
          airline-utf-glyph-readonly            #xe0a2
          airline-utf-glyph-linenumber          #xe0a1))

  (setq eshell-prompt-regexp "^ [^$]*[$] "
        eshell-prompt-function
        (lambda ()
          (concat
           (propertize
            (concat " " (airline-shorten-directory (eshell/pwd) 16) " ")
            'face `(:foreground ,(face-foreground 'airline-normal-outer)
                                :background ,(face-background 'airline-normal-outer)))

           (propertize
            (char-to-string airline-utf-glyph-separator-left)
            'face `(:foreground ,(face-background 'airline-normal-outer)
                                :background ,(face-background 'airline-normal-inner)))

           (let ((git-branch (airline-curr-dir-git-branch-string (eshell/pwd))))
             (if (not (or (null git-branch) (string= "" git-branch)))
                 (concat
                  (propertize
                   (concat " " (char-to-string airline-utf-glyph-branch) " " git-branch " ")
                   'face `(:foreground ,(face-background 'airline-insert-outer)
                                       :background ,(face-background 'airline-insert-inner))))))

           (propertize
            (char-to-string airline-utf-glyph-subseparator-left)
            'face `(:foreground ,(face-background 'airline-insert-outer)
                                :background ,(face-background 'airline-insert-inner)))

           (propertize "$" 'invisible t)
           (propertize " " 'face `())))))

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

;;;_ , allout

(use-package allout
  ;; BULK-ENSURE :ensure t
  :diminish allout-mode
  :commands allout-mode
  :config
  (progn
    (defvar allout-unprefixed-keybindings nil)

    (defun my-allout-mode-hook ()
      (dolist (mapping '((?b . allout-hide-bodies)
                         (?c . allout-hide-current-entry)
                         (?l . allout-hide-current-leaves)
                         (?i . allout-show-current-branches)
                         (?e . allout-show-entry)
                         (?o . allout-show-to-offshoot)))
        (bind-key (concat (format-kbd-macro allout-command-prefix)
                          " " (char-to-string (car mapping)))
                  (cdr mapping)
                  allout-mode-map))

      (if (memq major-mode lisp-modes)
          (unbind-key "C-k" allout-mode-map)))

    (add-hook 'allout-mode-hook 'my-allout-mode-hook)))

(use-package anakondo
  :unless noninteractive
  :commands anakondo-minor-mode
  :hook ((inf-clojure-mode
          clojure-mode
          clojurescript-mode
          clojurec-mode) . #'anakondo-minor-mode))

;;;_ , anzu

(use-package anzu
  ;; BULK-ENSURE :ensure t
  :diminish anzu-mode
  :config
  (global-anzu-mode 1))

;;;_ , ascii

(use-package ascii
  :unless install-run
  :commands (ascii-on ascii-toggle)
  :init
  (progn
    (defun ascii-toggle ()
      (interactive)
      (if ascii-display
          (ascii-off)
        (ascii-on)))

    (bind-key "C-c e A" 'ascii-toggle)))

;;;_ , archive-region

(use-package archive-region
  :commands kill-region-or-archive-region
  :bind ("C-w" . kill-region-or-archive-region))

;;;_ , async

(use-package async)
;; BULK-ENSURE :ensure t

(use-package atomic-chrome
  :custom
  (atomic-chrome-url-major-mode-alist
   '(("atlassian\\.net" . jira-markup-mode)
     ("reddit\\.com" . markdown-mode)
     ("github\\.com" . gfm-mode)
     ("redmine" . textile-mode))
   "Major modes for URLs.")
  :config
  (atomic-chrome-start-server))


;;;_ , auth-password-store

(use-package auth-password-store
  ;; TBD
  :disabled t
  :config
  (auth-pass-enable))

;;;_ , avy

(use-package avy
  :bind
  ("M-`" . avy-goto-char-2)
  :config
  (avy-setup-default))

;;;_ , auto-complete

(use-package auto-complete-config
  :disabled t ;; in favor of company-mode
  :commands auto-complete-mode
  :diminish auto-complete-mode
  :config
  (progn
    ;;(ac-set-trigger-key "TAB")
    (setq ac-use-menu-map t)

    (unbind-key "C-s" ac-completing-map)

    (defalias 'yas/current-snippet-table 'yas--get-snippet-tables)

    (setq-default ac-sources
                  '(
                    (if (featurep 'semantic)
                        'ac-source-semantic)
                    ac-source-yasnippet
                    ac-source-abbrev
                    ac-source-words-in-buffer
                    ac-source-words-in-same-mode-buffers
                    (if (featurep 'ac-nrepl)
                        'ac-nrepl)
                    ac-source-files-in-current-dir
                    ac-source-filename))))

;;;_ , auto-highlight-symbol-mode

(use-package auto-highlight-symbol
  :disabled t
  ;; BULK-ENSURE :ensure t
  :unless noninteractive
  :bind (:map auto-highlight-symbol-mode-map
              ("M-p"     . ahs-backward)
              ("M-n"     . ahs-forward)
              ("M-_"     . ahs-back-to-start)
              ("C-x M-_" . ahs-change-range)
              ("C-x C-a" . ahs-edit-mode))
  :custom
  (ahs-default-range 'ahs-range-whole-buffer)
  (ahs-face-check-include-overlay t)
  (ahs-inhibit-face-list
   '(font-lock-comment-delimiter-face
     font-lock-comment-face
     font-lock-doc-face
     font-lock-doc-string-face
     font-lock-string-face
     font-lock-keyword-face
     region
     loccur-custom-buffer-grep
     isearch))

  :hook (prog-mode . 'auto-highlight-symbol-mode)
  :config
  (set-face-attribute 'ahs-face nil
                      :background 'unspecified :box t)
  (set-face-attribute 'ahs-plugin-whole-buffer-face nil
                      :background 'unspecified
                      :foreground 'unspecified
                      :box '(:color 'GreenYellow))
  (add-hook 'yas-before-expand-snippet-hook
            (lambda () (auto-highlight-symbol-mode 0)))
  (add-hook 'yas-after-exit-snippet-hook
            (lambda () (when prog-mode-hook (auto-highlight-symbol-mode 1)))))

;;;_, autoinsert

(use-package autoinsert
  :disabled t
  :after yasnippet
  :unless noninteractive
  :preface
  (defun autoinsert-yas-expand ()
    "Replace text in yasnippet template."
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^# --.*$" nil t 1)
        (delete-region (point-min) (+ (point) 1))))
    (yas-expand-snippet (buffer-string) (point-min) (point-max)))

  (defun add-my-autoinsert-snippet (filename)
    "Add autoinsert rule for snippet FILENAME."
    (let ((ext  (file-name-extension    filename))
          (name (file-name-nondirectory filename)))
      (cond
       ((> (length ext) 0)
        (define-auto-insert (concat "\\." ext "$") (vector name 'autoinsert-yas-expand)))
       ((> (length name) 0)
        (define-auto-insert (concat "^" name) (vector name 'autoinsert-yas-expand)))
       (t (message "No auto-insert created for %s" filename)))))

  :custom
  (auto-insert-directory
   (ensure-directory (expand-file-name "autoinsert" common-emacs-directory)))
  (auto-insert 'other)
  (auto-insert-query nil)
  :config
  (mapc #'add-my-autoinsert-snippet
        (directory-files auto-insert-directory t
                         directory-files-no-dot-files-regexp))
  (auto-insert-mode +1))

;;;_ , autopair
;; using paredit instead

(use-package autopair
  :disabled t ;; in favor of paredit
  :commands autopair-mode
  :diminish autopair-mode
  :hook (c-mode-common . #'autopair-mode)
  :hook (text-mode . #'autopair-mode)
  :hook (ruby-mode . #'autopair-mode)
  :hook (python-mode . #'autopair-mode)
  :hook (go-mode . #'autopair-mode)
  :hook (js2-mode . #'autopair-mode)
  :hook (sh-mode . #'autopair-mode))

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

;;;_ , auto-yasnippet

(use-package auto-yasnippet
  ;; BULK-ENSURE :ensure t
  :after yasnippet
  :bind (("C-c y e" . aya-expand)
         ("C-c y c" . aya-create)))

;;;_ , base16-theme

(use-package base16-theme
  :config
  (load-theme 'base16-tomorrow-night t))

(use-package bazel-mode
  ;;; replaced by `bazel`
  :disabled t
  :mode "WORKSPACE"
  :mode "BUILD")

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
  :init
  (add-hook 'after-init-hook 'bm-repository-load)
  (add-hook 'find-file-hooks 'bm-buffer-restore)
  (add-hook 'after-revert-hook #'bm-buffer-restore)
  (add-hook 'kill-buffer-hook #'bm-buffer-save)
  (add-hook 'after-save-hook #'bm-buffer-save)
  (add-hook 'vc-before-checkin-hook #'bm-buffer-save)
  (add-hook 'kill-emacs-hook #'(lambda nil
                                 (bm-buffer-save-all)
                                 (bm-repository-save))))

;;;_ , bookmark+
;;

(use-package bookmark+
  ;; TBD must be downloaded by hand, no melpa
  :disabled t
  :init
  (progn
    (require 'bookmark)
    ;;! A `.txt' extension would load Org at the time `bookmark' is required!
    (setq bookmark-default-file "~/.emacs.d/bookmarks.bmk")
    (unless (file-exists-p bookmark-default-file)
      (bookmark-save)))
  :config
  (progn
    (defun leuven-bmkp-autoname-line (position)
      "Name autonamed bookmark at POSITION using line number."
      (let ((line  (line-number-at-pos position)))
        ;; (format "%s:%d (%s)" (buffer-name) line (buffer-file-name))
        (format "%s:%d: %s"
                (buffer-name)
                line
                (buffer-substring-no-properties
                 (line-beginning-position)
                 (1- (line-beginning-position 2))))))

    (setq bookmark-save-flag 1

          ;; Priorities of bookmark highlighting overlay types.
          bmkp-light-priorities '((bmkp-autonamed-overlays     . 150)
                                  (bmkp-non-autonamed-overlays . 160))

          ;; Symbols for the fringe bitmaps to use to highlight a bookmark.
          bmkp-light-left-fringe-bitmap 'filled-square
          bmkp-light-right-fringe-bitmap 'filled-square

          ;; Default highlight style for ANONYMOUS (= default) bookmarks.
          bmkp-light-style-autonamed 'line+lfringe

          ;; Default highlight style for bookmarks WITH MNEMONICS.
          bmkp-light-style-non-autonamed 'line+lfringe

          ;; Automatically highlight bookmarks when set.
          bmkp-auto-light-when-set 'all-in-buffer

          ;; Automatically highlight bookmarks when jumped to.
          bmkp-auto-light-when-jump 'all-in-buffer

          ;; Don't propertize bookmark names to hold full bookmark data.
          bmkp-propertize-bookmark-names-flag nil
          ;; We will often be going back and forth between using Bookmark+ and
          ;; using vanilla Emacs.

          bmkp-last-as-first-bookmark-file nil

          ;; Name ANONYMOUS bookmarks with buffer name and line number.
          ;; (setq bmkp-autoname-format "^%B:[0-9]+ (%s)")
          bmkp-autoname-format "^%B:[0-9]+: %s"

          bmkp-autoname-bookmark-function #'leuven-bmkp-autoname-line)))

;;;_ . browse-at-remote

(use-package browse-at-remote
  :defer t
  :custom
  (browse-at-remote-add-line-number-if-no-region-selected nil)
  :bind ("C-c g g" . browse-at-remote))


;;;_ , browse-kill-ring+

(use-package browse-kill-ring+
  :disabled t)

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

  ;; (add-hook 'erc-mode-hook
  ;;           (lambda ()
  ;;             (setq captain-predicate
  ;;                   (lambda ()
  ;;                     (let* ((gt1 (> (length (word-at-point)) 1))
  ;;                            (the-line (thing-at-point 'line t))
  ;;                            (laslash (= 0 (string-match "[[:alnum:]]+/$"
  ;;                                                        (reverse the-line)))))
  ;;                       (message "line: %s word: %s >1:%s %s"
  ;;                                the-line
  ;;                                (word-at-point)
  ;;                                (if gt1 "yes" "no")
  ;;                                (if laslash "yes" "no"))
  ;;                       gt1)))))

  (add-hook
   'org-mode-hook
   (lambda ()
     (setq captain-predicate
           (lambda () (not (org-in-src-block-p))))))
  (global-captain-mode +1))

;;;_ , cedet

(use-package cedet
  :disabled t
  :commands
  :init
  (progn
    ;; Enable Semantic
    (semantic-mode 1)
    (semantic-load-excessive-code-helpers)

    ;;(when nil              ; jww (2012-06-20): this kills buffers
    ;; if you want to enable support for gnu global
    (use-package semanticdb-global)
    (use-package semantic-ia)
    (use-package semantic-gcc)
    (use-package marshal)

    (use-package eassist
      :init
      (progn
        (defun local/c-mode-cedet-hook ()
          (bind-key "C-c t" 'eassist-switch-h-cpp)
          (bind-key "C-x t" 'eassist-switch-h-cpp)
          (bind-key "C-c e" 'eassist-list-methods)
          (bind-key "C-c C-r" 'semantic-symref))

        (add-hook 'c-mode-common-hook 'local/c-mode-cedet-hook))

      (global-semantic-tag-folding-mode 1)
      (semanticdb-enable-gnu-global-databases 'c-mode)
      (semanticdb-enable-gnu-global-databases 'c++-mode)

      (defun my-cedet-hook ()
        (bind-key [(control return)] 'semantic-ia-complete-symbol)
        (bind-key "C-c ?" 'semantic-ia-complete-symbol-menu)
        (bind-key "C-c >" 'semantic-complete-analyze-inline)
        (bind-key "C-c =" 'semantic-decoration-include-visit)
        (bind-key "C-c j" 'semantic-ia-fast-jump)
        (bind-key "C-c q" 'semantic-ia-show-doc)
        (bind-key "C-c s" 'semantic-ia-show-summary)
        (bind-key "C-c p" 'semantic-analyze-proto-impl-toggle)
        (bind-key "C-c +" 'semantic-tag-folding-show-block)
        (bind-key "C-c -" 'semantic-tag-folding-fold-block)
        (bind-key "C-c C-c +" 'semantic-tag-folding-show-all)
        (bind-key "C-c C-c -" 'semantic-tag-folding-fold-all))

      (add-hook 'c-mode-common-hook 'my-cedet-hook)

      ;; ctags
      (use-package semanticdb-ectag
        :init
        (progn
          (semantic-load-enable-primary-exuberent-ctags-support)))

      (global-semantic-idle-tag-highlight-mode 1)
      (when (cedet-ectag-version-check)
        (semantic-load-enable-primary-exuberent-ctags-support)))))

;;;_ , cider

(use-package cider
  :after clojure-mode
  :unless noninteractive
  ;; BULK-ENSURE :ensure t
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

;;;_ , cljr-helm

(use-package cljr-ivy)

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

;;;_ , company

(use-package company
  :defer 1
  :diminish " 𝍎"
  :commands (company-mode company-indent-or-complete-common)
  :custom
  (company-tooltip-align-annotations t)
  ;; (company-tooltip-flip-when-above t)
  (company-tooltip-limit 10)
  (company-require-match nil)
  (company-dabbrev-code-other-buffers t)
  (company-dabbrev-downcase t)
  (company-dabbrev-minimum-length 2)
  (company-dabbrev-ignore-case t)
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
  (company-show-numbers t)
  (company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
                       ;; company-echo-metadata-frontend
                       company-preview-if-just-one-frontend))
  (company-occurrence-weight-function
   #'company-occurrence-prefer-any-closest)
  (company-continue-commands
   '(append company-continue-commands
            '(comint-previous-matching-input-from-input
              comint-next-matching-input-from-input)))

  :init
  (dolist (hook '(prog-mode
                  emacs-lisp-mode-hook
                  haskell-mode-hook
                  c-mode-common-hook))
    (add-hook hook
              #'(lambda ()
                  (local-set-key (kbd "<tab>")
                                 #'company-indent-or-complete-common))))
  :config
  ;;(hook-into-modes 'company-mode 'prog-mode-hook 'erc-mode)
  ;; From https://github.com/company-mode/company-mode/issues/87
  ;; See also https://github.com/company-mode/company-mode/issues/123

  ;; (defadvice company-pseudo-tooltip-unless-just-one-frontend
  ;;     (around only-show-tooltip-when-invoked activate)
  ;;   (when (company-explicit-action-p)
  ;;     ad-do-it))

  ;; see http://oremacs.com/2017/12/27/company-numbers/

  (defun ora-company-number ()
    "Forward to `company-complete-number'.
Unless the number is potentially part of the candidate.
In that case, insert the number."
    (interactive)
    (let* ((k (this-command-keys))
           (re (concat "^" company-prefix k)))
      (if (or (cl-find-if (lambda (s) (string-match re s))
                          company-candidates)
              (> (string-to-number k)
                 (length company-candidates))
              (looking-back "[0-9]+\\.[0-9]*" (line-beginning-position)))
          (self-insert-command 1)
        (company-complete-number
         (if (equal k "0")
             10
           (string-to-number k))))))

  (defun ora--company-good-prefix-p (orig-fn prefix)
    (unless (and (stringp prefix) (string-match-p "\\`[0-9]+\\'" prefix))
      (funcall orig-fn prefix)))

  (advice-add 'company--good-prefix-p :around #'ora--company-good-prefix-p)

  (let ((map company-active-map))
    (mapc
     (lambda (x)
       (define-key map (format "%d" x) 'ora-company-number))
     (number-sequence 0 9))
    (define-key map " " (lambda ()
                          (interactive)
                          (company-abort)
                          (self-insert-command 1))))

  (defun check-expansion ()
    (save-excursion
      (if (outline-on-heading-p t)
          nil
        (if (looking-at "\\_>") t
          (backward-char 1)
          (if (looking-at "\\.") t
            (backward-char 1)
            (if (looking-at "->") t nil))))))

  (define-key company-mode-map [tab]
    '(menu-item "maybe-company-expand" nil
                :filter (lambda (&optional _)
                          (when (check-expansion)
                            #'company-complete-common))))

  (eval-after-load "yasnippet"
    '(progn
       (defun company-mode/backend-with-yas (backend)
         (if (and (listp backend) (member 'company-yasnippet backend))
             backend
           (append (if (consp backend) backend (list backend))
                   '(:with company-yasnippet))))
       (setq company-backends
             (mapcar #'company-mode/backend-with-yas company-backends))))

  (global-company-mode +1))


(use-package company-dict
  :after company
  :config
  (setq company-dict-dir (ensure-user-dir "dict/"))
  (cons 'company-dict company-backends))

(use-package company-ebdb
  :after company
  :config
  (cons 'company-ebdb company-backends))

(use-package company-emoji
  :disabled t
  ;; in favor of coompany-emojify
  :after company
  :config
  (progn
    (cons 'company-emoji company-backends)
    (set-fontset-font
     t 'symbol (font-spec :family "Symbola") nil 'prepend)))

(use-package company-emojify
  :after company
  :config
  (add-to-list 'company-backends 'company-emojify))

(use-package company-flx
  :after company
  :config
  (company-flx-mode +1))

(use-package company-go
  :after company)

(use-package company-jedi
  :disabled t
  :unless noninteractive
  :hook
  ((python-mode . jedi:setup))
  :init
  (setq jedi:complete-on-dot t)
  (setq jedi:use-shortcuts t)
  (add-hook 'python-mode-hook
            (lambda () (add-to-list 'company-backends 'company-jedi))))

(use-package company-math
  :after company
  :defer t
  :config
  (add-to-list 'company-backends 'company-math-symbols-unicode))

(use-package company-ngram
  :disabled t
  :after company
  :config
  (progn
    (setq company-ngram-data-dir
          (locate-user-emacs-file "data/ngram"))
    ;; company-ngram supports python 3 or newer
    company-ngram-python "python3"
    (company-ngram-init)
    (add-to-list 'company-backends #'company-ngram-backend)
    ;; or use `M-x turn-on-company-ngram' and
    ;; `M-x turn-off-company-ngram' on individual buffers

    ;; save the cache of candidates
    (run-with-idle-timer 7200 t
                         (lambda (
                             (company-ngram-command "save_cache"))))))

(use-package company-org-block
  :after org
  :custom
  (company-org-block-edit-style 'auto) ;; 'auto, 'prompt, or 'inline
  :hook ((org-mode . (lambda ()
                       (setq-local company-backends '(company-org-block))
                       (company-mode +1)))))

(use-package company-php
  :after company
  :config
  (add-hook 'php-mode-hook
            '(lambda ()
               (require 'company-php)
               (company-mode t)
               (add-to-list 'company-backends 'company-ac-php-backend))))

(use-package company-quickhelp
  :after company
  :custom
  (company-quickhelp-use-propertized-text t)
  (company-quickhelp-delay 0)
  :config (company-quickhelp-mode +1))

(use-package company-shell
  :after company
  :config
  (progn
    (add-to-list 'company-backends 'company-shell)
    (add-to-list 'company-backends 'company-shell-env)))

(use-package company-statistics
  :after company
  :config
  (company-statistics-mode))

(use-package company-web
  :after company
  :preface

  :config
  (add-hook
   'web-mode-hook (lambda ()
                    (set (make-local-variable 'company-backends)
                         '(company-web-html))
                    (company-mode t))))

;;;_ , compile

(use-package compile
  :disabled
  :bind (("C-c c" . compile)
         ("M-O"   . show-compilation))
  :preface
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

  (defun compilation-ansi-color-process-output ()
    (ansi-color-process-output nil)
    (set (make-local-variable 'comint-last-output-start)
         (point-marker)))

  :config
  (add-hook 'compilation-filter-hook #'compilation-ansi-color-process-output))

(use-package color-identifiers-mode
  :unless noninteractive
  ;; BULK-ENSURE :ensure t
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

;;;_ , color-theme

(use-package color-theme
  :disabled t
  :unless noninteractive
  ;; BULK-ENSURE :ensure t
  :config
  (color-theme-initialize)
  (color-theme-charcoal-black))

(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode +1))

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
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c b" . consult-bookmark)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command) ;; orig. repeat-complex-command
         ("C-\\" . consult-buffer)
         ("C-x b" . consult-buffer) ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame) ;; orig. switch-to-buffer-other-frame
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store) ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)     ;; orig. yank-pop
         ("<help> a" . consult-apropos) ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake) ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)   ;; orig. goto-line
         ("M-g M-g" . consult-goto-line) ;; orig. goto-line
         ("M-g o" . consult-outline) ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)
         ("M-s F" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch)
         :map isearch-mode-map
         ("M-e" . consult-isearch)   ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch) ;; orig. isearch-edit-string
         ("M-s l" . consult-line) ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)) ;; needed by consult-line to detect isearch

  ;; Enable automatic preview at point in the *Completions* buffer.
  ;; This is relevant when you use the default completion UI,
  ;; and not necessary for Vertico, Selectrum, etc.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Optionally replace `completing-read-multiple' with an enhanced version.
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :custom
  (consult-buffer-sources '(consult--source-hidden-buffer
                            consult--source-buffer consult--source-recent-file
                            consult--source-bookmark))
  ;; removed consult--source-project-buffer consult--source-project-recent-file

  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
   :preview-key (kbd "M-."))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; Optionally configure a function which returns the project root directory.
  ;; There are multiple reasonable alternatives to chose from.
;;;; 1. project.el (project-roots)
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project)))))
;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-root-function #'projectile-project-root)
;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-root-function #'vc-root-dir)
;;;; 4. locate-dominating-file
  ;; (setq consult-project-root-function (lambda () (locate-dominating-file "." ".git")))
  )

(use-package consult-dir
  :after consult
  :bind
  (:map vertico-map
        ("M-." . consult-dir)
        ("M-j" . consult-dir-jump-file)))

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


;;;_ , dedicated

(use-package dedicated
  :defer t)

;;;_ , deft

(use-package deft
  :commands deft
  :config
  (progn
    (setq deft-default-extension "org")
    (setq deft-extensions '("org"))
    (setq deft-use-filter-string-for-filename t)
    (setq deft-file-naming-rules '((noslash . "_")
                                   (nospace . "_")
                                   (case-fn . downcase)))
    (setq deft-directory "~/SparkleShare/org/notes")))

;;;_ , diff-mode

(use-package diff-mode
  :commands diff-mode)

;;;_ , dired

(use-package dired
  :straight nil
  :bind ("C-c J" . dired-double-jump)
  :preface
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

  (defun dired-double-jump (first-dir second-dir)
    (interactive
     (list (read-directory-name "First directory: "
                                (expand-file-name "~")
                                nil nil "dl/")
           (read-directory-name "Second directory: "
                                (expand-file-name "~")
                                nil nil "Archives/")))
    (dired first-dir)
    (dired-other-window second-dir))

  (defun my-dired-switch-window ()
    (interactive)
    (if (eq major-mode 'sr-mode)
        (call-interactively #'sr-change-window)
      (call-interactively #'other-window)))

  :custom
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  (dired-deletion-confirmer 'y-or-n-p)
  (dired-clean-up-buffers-too nil)
  (delete-by-moving-to-trash t)
  ;; trash-directory "~/.Trash/emacs"
  (dired-dwim-target t)
  (dired-guess-shell-alist-user
   '(("\\.pdf\\'" "evince")
     ("\\.jpg\\'" "feh")))
  (dired-listing-switches "-alv")
  :config
  (bind-key "l" #'dired-up-directory dired-mode-map)

  (bind-key "<tab>" #'my-dired-switch-window dired-mode-map)

  (bind-key "M-!" #'async-shell-command dired-mode-map)
  (unbind-key "M-G" dired-mode-map)

  (use-package dired-narrow
    :defer t
    ;; BULK-ENSURE :ensure t
    :bind (:map dired-mode-map
                ("/" . dired-narrow)))

  ;; (fset 'asynch-play-media-using-last-command
  ;;       [?& up return])

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

  (bind-key ";" #'dired-do-repeat-shell-command dired-mode-map)

  ;; (setq my-dired-omit-files
  ;;    '("^\\.$"
  ;;      "^\\.\\.$"
  ;;      "^\\.git$")
  ;;    )
  ;; (setq dired-omit-files
  ;;    (mapconcat #'identity my-dired-omit-files "\\|"))

  ;; (add-hook 'dired-mode-hook (lambda () (dired-omit-mode)))

  (use-package dired+
    :disabled t
    :config
    (unbind-key "M-s f" dired-mode-map))

  (use-package dired-details
    ;; (shell-command "rm -f site-lisp/dired-details.el*")
    :disabled t)

  (use-package dired-ranger
    ;; BULK-ENSURE :ensure t
    :defer t
    :bind (:map dired-mode-map
                ("W" . dired-ranger-copy)
                ("X" . dired-ranger-move)
                ("Y" . dired-ranger-paste)))

  (use-package dired-toggle
    :disabled t
    :bind (("C-c C-T" . #'dired-toggle)
           :map dired-mode-map
           ("q" . #'dired-toggle-quit)
           ([remap dired-find-file] . #'dired-toggle-find-file)
           ([remap dired-up-directory] . #'dired-toggle-up-directory)
           ("C-c C-u" . #'dired-toggle-up-directory))
    :config
    (setq dired-toggle-window-size 32)
    (setq dired-toggle-window-side 'left)

    ;; Optional, enable =visual-line-mode= for our narrow dired buffer:
    (add-hook 'dired-toggle-mode-hook
              (lambda () (interactive)
                (visual-line-mode 1)
                (setq-local visual-line-fringe-indicators '(nil right-curly-arrow))
                (setq-local word-wrap nil))))

  ;; (defadvice dired-omit-startup (after diminish-dired-omit activate)
  ;;   "Make sure to remove \"Omit\" from the modeline."
  ;;   (diminish 'dired-omit-mode) dired-mode-map)

  ;; (defadvice dired-next-line (around dired-next-line+ activate)
  ;;   "Replace current buffer if file is a directory."
  ;;   ad-do-it
  ;;   (while (and  (not  (eobp)) (not ad-return-value))
  ;;     (forward-line)
  ;;     (setq ad-return-value(dired-move-to-filename)))
  ;;   (when (eobp)
  ;;     (forward-line -1)
  ;;     (setq ad-return-value(dired-move-to-filename))))

  ;; (defadvice dired-previous-line (around dired-previous-line+ activate)
  ;;   "Replace current buffer if file is a directory."
  ;;   ad-do-it
  ;;   (while (and  (not  (bobp)) (not ad-return-value))
  ;;     (forward-line -1)
  ;;     (setq ad-return-value(dired-move-to-filename)))
  ;;   (when (bobp)
  ;;     (call-interactively 'dired-next-line)))

  (defvar dired-omit-regexp-orig (symbol-function 'dired-omit-regexp))

  ;; Omit files that Git would ignore
  (defun dired-omit-regexp ()
    (let ((file (expand-file-name ".git"))
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
        (setq file (expand-file-name ".git" parent-dir)))
      ;; If we found a change log in a parent, use that.
      (if (file-exists-p file)
          (let ((regexp (funcall dired-omit-regexp-orig))
                (omitted-files
                 (shell-command-to-string "git clean -d -x -n")))
            (if (= 0 (length omitted-files))
                regexp
              (concat
               regexp
               (if (> (length regexp) 0)
                   "\\|" "")
               "\\("
               (mapconcat
                #'(lambda (str)
                    (concat
                     "^"
                     (regexp-quote
                      (substring str 13
                                 (if (= ?/ (aref str (1- (length str))))
                                     (1- (length str))
                                   nil)))
                     "$"))
                (split-string omitted-files "\n" t)
                "\\|")
               "\\)")))
        (funcall dired-omit-regexp-orig)))))

(use-package dired-git
  :hook (dired-mode-hook . #'dired-git-mode))

;;;_ , disable-mouse-mode

(use-package disable-mouse
  :diminish disable-mouse-global-mode
  :config
  (global-disable-mouse-mode +1))

;; try this alternative
;; (defun turn-off-mouse (&optional frame)
;;   (interactive)
;;   (let ((inhibit-message t) (default-directory "~"))
;;     (shell-command "synclient TouchpadOff=1")))

;; (defun turn-on-mouse (&optional frame)
;;   (interactive)
;;   (let ((inhibit-message t) (default-directory "~"))
;;     (shell-command "synclient TouchpadOff=0")))

;; (add-hook 'focus-in-hook #'turn-off-mouse)
;; (add-hook 'focus-out-hook #'turn-on-mouse)
;; (add-hook 'delete-frame-functions #'turn-on-mouse)

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

(use-package docker-tramp
  :after tramp
  :defer 5)

;;;_ , dockerfile

(use-package dockerfile-mode
  ;; BULK-ENSURE :ensure t
  :mode "Dockerfile[a-zA-Z.-]*\\'")

;;;_ , docker-tramp

(use-package docker-tramp)
;; BULK-ENSURE :ensure t


;;;_ , doom-themes

(use-package doom-themes
  :disabled t ;; in favor of base-16
  :load-path ("~/emacs-doom-themes/" "~/emacs-doom-themes/themes/")
  :config
  (progn
    (load-theme 'doom-tomorrow-night t)
    (doom-themes-neotree-config)
    (doom-themes-org-config)))

(use-package doom-modeline
  ;; BULK-ENSURE :ensure t
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

;;;_ , doxymacs

(use-package doxymacs
  :disabled t
  :load-path "site-lisp/doxymacs/lisp/")

;;;_ , dumb-jump

(use-package dumb-jump
  ;; BULK-ENSURE :ensure t
  :bind (("M-g o" . dumb-jump-go-other-window))
  ("M-g j" . dumb-jump-go)
  ("M-g b" . dumb-jump-back)
  ("M-g x" . dumb-jump-go-prefer-external)
  ("M-g z" . dumb-jump-go-prefer-external-other-window)
  :config
  (setq dumb-jump-selector 'ivy))


;;;_ , dynamic-spaces

(use-package dynamic-spaces
  ;; BULK-ENSURE :ensure t
  :config
  (dynamic-spaces-global-mode 1))

(use-package easy-kill
  :unless noninteractive
  ;; BULK-ENSURE :ensure t
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


;;;_ , eclim

(use-package eclim
  :disabled t
  :config
  (setq eclim-eclipse-dirs '("/usr/lib64/eclipse"))
  (setq eclim-executable (concat)
        "/home/emacs/.eclipse"
        "/org.eclipse.platform_793567567_linux_gtk_x86_64/"
        "eclim")
  (setq help-at-pt-display-when-idle t)
  (setq help-at-pt-timer-delay 0.1)
  (help-at-pt-set-timer)
  (global-eclim-mode)
  (use-package maven-test-mode)
  (use-package eclimd
    :config
    (setq eclimd-executable (concat)
          "/home/emacs/.eclipse"
          "/org.eclipse.platform_793567567_linux_gtk_x86_64/"
          "eclimd"))

  (use-package company-emacs-eclim
    :config
    (company-emacs-eclim-setup)))

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
  :init
  (defun test-compare ()
    (interactive)
    (delete-other-windows)
    (let ((here (point)))
      (search-forward "got:")
      (split-window-below)
      (goto-char here))
    (search-forward "expected:")
    (call-interactively #'compare-windows)))

(use-package edit-indirect
  :bind (("C-c '" . edit-indirect-region)))

;;;_ , edit-indirect

(use-package edit-indirect
  :bind ("C-c C" . indirect-region))

;;;_ , edit-server

;; (use-package edit-server
;;   :init
;;   (progn
;;     (add-hook 'after-init-hook 'server-start t)
;;     (add-hook 'after-init-hook 'edit-server-start t)))

(use-package eglot
  :ensure t
  :commands (eglot eglot-ensure)
  :hook ((swift-mode . eglot-ensure)
         (clojure-mode . eglot-ensure)
         (clojurec-mode . eglot-ensure)
         (clojurescript-mode . eglot-ensure)
         (scala-mode . eglot-ensure)
         (python-mode . eglot-ensure)
         (java-mode . eglot-ensure)
         (go-mode . eglot-ensure)
         (yaml-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
         (c-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (obc-c-mode . eglot-ensure))
  :custom
  (eglot-confirm-server-initiated-edits nil))
  ;;; inherited cruft, not sure if needed
  ;; :config
  ;; (setq pd-toolchain-directory "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/")
  ;; (setq pd-clangd-path (pcase system-type
  ;;                        (`darwin "/opt/homebrew/opt/llvm/bin/clangd")
  ;;                        (_ "clangd")))
  ;; (setq pd-sourcekit-lsp-path (pcase system-type
  ;;                               (`darwin (concat pd-toolchain-directory "usr/bin/sourcekit-lsp"))
  ;;                               (_ "sourcekit-lsp")))

  ;; (defun pd-cc-mode-lsp-server (arg)
  ;;   "Figure if we want to use sourcekit-lsp or clangd as our server."
  ;;   (let* ((root (nth 0 (project-roots (project-current))))
  ;;          (swift-package-file (concat root "Package.swift"))
  ;;          (cmake-file (concat root "CMakeLists.txt")))
  ;;     (if (and (not (file-exists-p cmake-file)) (file-exists-p swift-package-file) (file-exists-p pd-sourcekit-lsp-path))
  ;;         (list pd-sourcekit-lsp-path)
  ;;       (list pd-clangd-path))))

  ;; (add-to-list 'eglot-server-programs '((swift-mode) . (pd-sourcekit-lsp-path)))
  ;; (add-to-list 'eglot-server-programs '((c-mode c++-mode obj-c-mode) . pd-cc-mode-lsp-server))
  ;; (add-to-list 'eglot-server-programs '(rust-mode "rust-analyzer")))

;;;_ , elfeed

(use-package elfeed
  :unless noninteractive
  ;; BULK-ENSURE :ensure t
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

;;;_ , elfeed-org

(use-package elfeed-org
  :unless noninteractive
  :after org
  :config
  (setq rmh-elfeed-org-files
        (directory-files
         (ensure-user-dir "elfeed-org") t "\\.org$"))
  (elfeed-org))

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

;;;_ , emacs-snippets

(use-package emacs-snippets
  :disabled t
  :unless noninteractive
  :after yasnippet
  ;; :load-path "/home/emacs/emacs-snippets/"
  :config
  (yas-reload-all))

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
  :preface
  (require 'erc)
  (require 'erc-sound)
  (erc-sound-enable)
  :init
  (use-package erc-bitlbee-twitter-decorate
    :disabled t
    :load-path "/home/emacs/.emacs.d/lisp"
    :config (erc-bitlbee-twitter-decorate-mode 1))

  (ensure-user-dir "erc/logs")
  :config
  (company-mode -1)
  (erc-spelling-mode +1)
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

  (setq erc-header-line-format nil
        erc-input-line-position -1
        erc-show-my-nick 1
        erc-timestamp-only-if-changed-flag t
        erc-timestamp-format "[%H:%M] "
        erc-datestamp-format " === [%Y-%m-%d %a] ===\n" ; mandatory ascii art
        erc-fill-prefix "      "
        erc-query-display 'buffer
        erc-insert-timestamp-function #'ks-timestamp)

  (setq erc-fill-column 180
        erc-fill-function 'erc-fill-static
        erc-fill-static-center 16)

  (set-face-attribute 'erc-default-face nil
                      :font "Roboto Condensed" :width 'condensed)
  (add-hook 'erc-send-pre-hook #'define-abbrev-sedlike)

  (bind-key "<tab>" 'completion-at-point erc-mode-map)
  (bind-key "C-s" 'isearch-forward erc-mode-map)
  (bind-key "C-r" 'isearch-backward erc-mode-map)
  (erc-track-minor-mode 1)
  (erc-track-mode 1)

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

  (defun erc-receiverize-prompt ()
    (if (and (boundp 'erc-default-recipients)
             (erc-default-target))
        (erc-propertize (concat (erc-default-target) ">")
                        'read-only t 'rear-nonsticky t
                        'front-nonsticky t)
      (erc-propertize (concat "ERC>") 'read-only t
                      'rear-nonsticky t
                      'front-nonsticky t)))

  (defun erc-cmd-CLEAR ()
    "Clears the current buffer"
    (erc-truncate-buffer-to-size 0))

  (defun erc-cmd-CLEARALL ()
    "Clears all ERC buffers"
    (setq erc-modified-channels-alist '())
    (mapc (lambda (buffer)
            (erc-truncate-buffer-to-size 0 (get-buffer buffer)))
          (erc-all-buffer-names)))

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

;;;_ , eshell

(use-package eshell
  :commands (eshell eshell-command)
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

(use-package esh-toggle
  :disabled t ; in favor of `eshell-toggle’
  :bind ("C-x C-z" . eshell-toggle))

(use-package eshell-bookmark
  :hook (eshell-mode . eshell-bookmark-setup))

(use-package eshell-git-prompt
  :config
  (eshell-git-prompt-use-theme 'powerline))

(use-package eshell-up
  :commands eshell-up)

(use-package eshell-z
  :after eshell)

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
  :bind (("C-c i b" . flyspell-buffer)
         ("C-c i f" . flyspell-mode))
  :commands flyspell-mode
  :init (progn
          ;; (add-hook 'prog-mode-hook #'flyspell-prog-mode)
          (dolist (mode-hook '(text-mode-hook org-mode-hook LaTeX-mode-hook))
            (add-hook mode-hook #'flyspell-mode)))
  ;; :hook (prog-mode . 'flyspell-prog-mode)
  ;; :hook (text-mode . 'flyspell-mode)
  :custom
  (flyspell-abbrev-p t)
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

;;;_ , fold-dwim

(use-package fold-dwim
  :defer t
  :bind (("<f13>" . fold-dwim-toggle)
         ("<f14>" . fold-dwim-hide-all)
         ("<f15>" . fold-dwim-show-all)))

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
  :custom
  (goggles-pulse t)
  :config
  (goggles-mode))

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

;;;_ , helm

;; (defvar helm-alive-p nil)

;; (use-package helm-config
;;
;;   :init
;;   (progn
;;     (bind-key "C-c M-x" 'helm-M-x)
;;     (bind-key "C-h a" 'helm-c-apropos)
;;     (bind-key "M-s a" 'helm-do-grep)
;;     (bind-key "M-s b" 'helm-occur)
;;     (bind-key "M-s F" 'helm-for-files)

;;     (use-package helm-commands)

;;     (use-package helm-flycheck
;;       :bind ("C-c ! h" . helm-flycheck))

;;     (bind-key "C-h e a" 'my-helm-apropos)
;;     (bind-key "C-x M-!" 'helm-command-from-zsh)
;;     (bind-key "C-x f" 'helm-find-git-file)

;;     (use-package helm-descbinds
;;       :commands helm-descbinds
;;       :init
;;       (fset 'describe-bindings 'helm-descbinds))

;;     (bind-key "C-h b" 'helm-descbinds))

;;   :config
;;   (helm-match-plugin-mode t))

;;;_ , help-mode

(use-package help-mode+
  :defer t
  :bind (:map help-mode-map
              ("<tab>" . forward-button)))

;;;_ , helpful

(use-package helpful
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)
   ("C-c C-d" . helpful-at-point)
   ("C-h F" . helpful-function)
   ("C-h C" . helpful-command)))

;;;_ , hi-lock

;; (use-package hi-lock
;;   :bind (("M-o l" . highlight-lines-matching-regexp)
;;          ("M-o r" . highlight-regexp)
;;          ("M-o w" . highlight-phrase)))

;;;_ , hilit-chg

;; (require 'hilit-chg)
;; (global-highlight-changes-mode t)
;; (set-face-attribute 'highlight-changes nil :background "gray23" :foreground 'unspecified)
;; (use-package hilit-chg
;;   :bind ("M-o C" . highlight-changes-mode))

;;;_ , hl-line

;; (use-package hl-line
;;   :bind ("M-o h" . hl-line-mode)
;;   :config
;;   (use-package hl-line+))

;;;_ , highlight-sexp

;; (use-package highlight-sexp
;;   :disabled t)

;;;_ , highlight-symbol

(use-package highlight-symbol
  :disabled t ;; in favor of symbol-overlay
  :hook ('prog-mode . 'highlight-symbol-mode)
  :config
  (progn
    (setq highlight-symbol-on-navigation-p t)

    (add-hook 'yas-before-expand-snippet-hook
              (lambda () (highlight-symbol-mode 0)))
    (add-hook 'yas-after-exit-snippet-hook
              (lambda () (when prog-mode-hook (highlight-symbol-mode 1))))
    (bind-keys
     :map auto-highlight-symbol-mode-map
     ("M-<"     . highlight-symbol-prev)
     ("M->"     . highlight-symbol-next))))

;; disabled in favor of auto-highlight-symbol-mode because ahs has a separate
;; keymap for navigation so keys can be overloaded

;; (use-package highlight-symbol
;;   :bind (("M-<" . (highlight-symbol-jump -1))
;;          ("M->" . ( highlight-symbol-jump 1)))
;;   :init
;;   (hook-into-modes 'highlight-symbol-mode 'prog-mode-hook))

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

  (defun my-try-expand-company (old)
    (unless company-candidates
      (company-auto-begin))
    (if (not old)
        (progn
          (he-init-string (he-lisp-symbol-beg) (point))
          (if (not (he-string-member he-search-string he-tried-table))
              (setq he-tried-table (cons he-search-string he-tried-table)))
          (setq he-expand-list
                (and (not (equal he-search-string ""))
                     company-candidates))))
    (while (and he-expand-list
                (he-string-member (car he-expand-list) he-tried-table))
      (setq he-expand-list (cdr he-expand-list)))
    (if (null he-expand-list)
        (progn
          (if old (he-reset-string))
          ())
      (progn
        (he-substitute-string (car he-expand-list))
        (setq he-expand-list (cdr he-expand-list))
        t)))

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
  :defer t
  :bind ("C-x C-b" . ibuffer)
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

;; (use-package image-file
;;   :disabled t
;;   :init
;;   (auto-image-file-mode 1))

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
  :bind ("C-h C-i" . info-lookup-symbol))

(use-package info-look
  :init
  (autoload 'info-lookup-add-help "info-look"))

(use-package info-lookmore
  :disabled t
  :after info-look
  :config
  (info-lookmore-elisp-cl)
  (info-lookmore-elisp-userlast)
  (info-lookmore-elisp-gnus)
  (info-lookmore-apropos-elisp))

;; (use-package info-look
;;   :commands info-lookup-add-help)

;;;_ , ipa

(use-package ipa
  :disabled t
  :commands ipa-insert
  :init
  (progn
    (autoload 'ipa-load-annotations-into-buffer "ipa")
    (add-hook 'find-file-hook 'ipa-load-annotations-into-buffer)))

;;;_ , ivy-mode

(use-package ivy
  :disabled t
  :preface
  (defun ivy-done-or-delete-char ()
    (interactive)
    (call-interactively
     (if (eolp)
         #'ivy-immediate-done
       #'ivy-delete-char)))

  (defun ivy-alt-done-or-space ()
    (interactive)
    (call-interactively
     (if (= ivy--length 1)
         #'ivy-alt-done
       #'self-insert-command)))

  (defun ivy-switch-buffer-kill ()
    (interactive)
    (debug)
    (let ((bn (ivy-state-current ivy-last)))
      (when (get-buffer bn)
        (kill-buffer bn))
      (unless (buffer-live-p (ivy-state-buffer ivy-last))
        (setf (ivy-state-buffer ivy-last)
              (with-ivy-window (current-buffer))))
      (setq ivy--all-candidates (delete bn ivy--all-candidates))
      (ivy--exhibit)))

  ;; This is the value of `magit-completing-read-function', so that we see
  ;; Magit's own sorting choices.
  (defun my-ivy-completing-read (&rest args)
    (let ((ivy-sort-functions-alist '((t . nil))))
      (apply 'ivy-completing-read args)))

  :bind (("C-c C-r". ivy-resume)
         ("C-x C-f" . counsel-find-file)
         ("C-x b" . ivy-switch-buffer)
         ("C-c C-r" . ivy-resume)
         ("C-c C-v p" . ivy-push-view)
         ("C-c C-v o" . ivy-pop-view)
         ("C-c C-v C-|" . ivy-switch-view))

  :bind (:map ivy-minibuffer-map
              ("<tab>" . ivy-alt-done)
              ("SPC"   . ivy-alt-done-or-space)
              ("C-d"   . ivy-done-or-delete-char)
              ("C-i"   . ivy-partial-or-done)
              ("C-r"   . ivy-previous-line-or-history)
              ("M-r"   . ivy-reverse-i-search))

  :diminish ivy-mode
  :custom
  (ivy-dynamic-exhibit-delay-ms 200)
  (ivy-height 10)
  (ivy-initial-inputs-alist nil t)
  (ivy-magic-tilde nil)
  (ivy-use-virtual-buffers t)
  (ivy-wrap t)
  (ivy-count-format "(%d/%d) ")
  :config
  (ivy-mode +1))

(use-package counsel
  :disabled t
  :after ivy org
  :diminish counsel-mode
  :init
  (counsel-mode +1)
  :custom
  (counsel-yank-pop-preselect-last t)
  (counsel-search-engine 'google)
  :bind
  (("M-y" . counsel-yank-pop)
   :map ivy-minibuffer-map
   ("M-y" . ivy-next-line)
   :map counsel-mode-map
   ([remap swiper] . counsel-grep-or-swiper)
   ([remap dired] . counsel-dired)
   ("C-x C-r" . counsel-recentf)
   ("C-x j" . counsel-mark-ring)
   ("C-h F" . counsel-describe-face)

   ("C-c L" . counsel-load-library)
   ("C-c P" . counsel-package)
   ("C-c f" . counsel-find-library)
   ("C-c g" . counsel-grep)
   ("C-c h" . counsel-command-history)
   ("C-c j" . counsel-git-grep)
   ("C-c l" . counsel-locate)
   ("C-c r" . counsel-rg)
   ("C-c z" . counsel-fzf)

   ("C-c c F" . counsel-faces)
   ("C-c c L" . counsel-load-library)
   ("C-c c P" . counsel-package)
   ("C-c c a" . counsel-apropos)
   ("C-c c e" . counsel-colors-emacs)
   ("C-c c f" . counsel-find-library)
   ("C-c c g" . counsel-grep)
   ("C-c c h" . counsel-command-history)
   ("C-c c i" . counsel-git)
   ("C-c c j" . counsel-git-grep)
   ("C-c c l" . counsel-locate)
   ("C-c c m" . counsel-minibuffer-history)
   ("C-c c o" . counsel-outline)
   ("C-c c p" . counsel-pt)
   ("C-c c r" . counsel-rg)
   ("C-c c s" . counsel-ag)
   ("C-c c t" . counsel-load-theme)
   ("C-c c u" . counsel-unicode-char)
   ("C-c c w" . counsel-colors-web)
   ("C-c c z" . counsel-fzf)))

(use-package counsel-projectile
  :disabled t
  :after (counsel projectile)
  :config
  ;; open project in vc after switching
  (counsel-projectile-modify-action
   'counsel-projectile-switch-project-action
   '((default counsel-projectile-switch-project-action-vc)))
  (counsel-projectile-mode))

(use-package counsel-flycheck
  :disabled t
  :straight nil
  :after counsel flycheck
  :unless noninteractive
  :preface
  (defvar counsel-flycheck-history nil
    "History for `counsel-flycheck'")

  (defun counsel-flycheck ()
    "Navigate to flycheck errors"
    (interactive)
    (if (not (bound-and-true-p flycheck-mode))
        (message "Flycheck mode is not available or enabled")
      (ivy-read "Error: "
                (let ((source-buffer (current-buffer)))
                  (with-current-buffer
                      (or (get-buffer flycheck-error-list-buffer)
                          (progn
                            (with-current-buffer
                                (get-buffer-create flycheck-error-list-buffer)
                              (flycheck-error-list-mode)
                              (current-buffer))))
                    (flycheck-error-list-set-source source-buffer)
                    (flycheck-error-list-reset-filter)
                    (revert-buffer t t t)
                    (split-string (buffer-string) "\n" t " *")))
                :action
                (lambda (s &rest _)
                  (when-let*
                      ((the-error (get-text-property 0 'tabulated-list-id s))
                       (pos (flycheck-error-pos the-error)))
                    (goto-char (flycheck-error-pos the-error))))
                :history 'counsel-flycheck-history)))
  :commands (counsel-flycheck)
  :bind ("C-!" . #'counsel-flycheck))

(use-package counsel-ebdb
  :disabled t)

(use-package counsel-etags
  :disabled t
  :bind ("C-c C-t" . counsel-etags-find-tag-at-point))

(use-package ivy-emoji
  :disabled t
  :bind ("C-c i e" . ivy-emoji) ;; mnemonics i e = insert emoji
  )

(use-package ivy-rich
  :disabled t
  :after ivy
  :init
  (defun ivy-rich-bookmark-name (candidate)
    (car (assoc candidate bookmark-alist)))

  (defun ivy-rich-buffer-icon (candidate)
    "Display buffer icons in `ivy-rich'."
    (when (display-graphic-p)
      (let* ((buffer (get-buffer candidate))
             (buffer-file-name (buffer-file-name buffer))
             (major-mode (buffer-local-value 'major-mode buffer))
             (icon (if (and buffer-file-name
                            (all-the-icons-auto-mode-match?))
                       (all-the-icons-icon-for-file
                        (file-name-nondirectory buffer-file-name)
                        :v-adjust -0.05)
                     (all-the-icons-icon-for-mode
                      major-mode :v-adjust -0.05))))
        (if (symbolp icon)
            (all-the-icons-faicon
             "file-o" :face 'all-the-icons-dsilver :height 0.8 :v-adjust 0.0)
          icon))))

  (defun ivy-rich-file-icon (candidate)
    "Display file icons in `ivy-rich'."
    (when (display-graphic-p)
      (let* ((path (file-local-name (concat ivy--directory candidate)))
             (file (file-name-nondirectory path))
             (icon (cond
                    ((file-directory-p path)
                     (cond
                      ((and (fboundp 'tramp-tramp-file-p)
                            (tramp-tramp-file-p default-directory))
                       (all-the-icons-octicon
                        "file-directory" :height 1.0 :v-adjust 0.01))
                      ((file-symlink-p path)
                       (all-the-icons-octicon
                        "file-symlink-directory" :height 1.0 :v-adjust 0.01))
                      ((all-the-icons-dir-is-submodule path)
                       (all-the-icons-octicon
                        "file-submodule" :height 1.0 :v-adjust 0.01))
                      ((file-exists-p (format "%s/.git" path))
                       (all-the-icons-octicon
                        "repo" :height 1.1 :v-adjust 0.01))
                      (t (let ((matcher (all-the-icons-match-to-alist
                                         path all-the-icons-dir-icon-alist)))
                           (apply (car matcher)
                                  (list (cadr matcher) :v-adjust 0.01))))))
                    ((string-match "^/.*:$" path)
                     (all-the-icons-material
                      "settings_remote" :height 1.0 :v-adjust -0.2))
                    ((not (string-empty-p file))
                     (all-the-icons-icon-for-file file :v-adjust -0.05)))))
        (if (symbolp icon)
            (all-the-icons-faicon
             "file-o" :face 'all-the-icons-dsilver :height 0.8 :v-adjust 0.0)
          icon))))

  (defun ivy-rich-dir-icon (candidate)
    "Display directory icons in `ivy-rich'."
    (when (display-graphic-p)
      (all-the-icons-octicon "file-directory" :height 1.0 :v-adjust 0.01)))

  (defun ivy-rich-function-icon (_candidate)
    "Display function icons in `ivy-rich'."
    (when (display-graphic-p)
      (all-the-icons-faicon
       "cube" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-purple)))

  (defun ivy-rich-variable-icon (_candidate)
    "Display variable icons in `ivy-rich'."
    (when (display-graphic-p)
      (all-the-icons-faicon
       "tag" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-lblue)))

  (defun ivy-rich-symbol-icon (_candidate)
    "Display symbol icons in `ivy-rich'."
    (when (display-graphic-p)
      (all-the-icons-octicon
       "gear" :height 0.9 :v-adjust -0.05)))

  (defun ivy-rich-theme-icon (_candidate)
    "Display theme icons in `ivy-rich'."
    (when (display-graphic-p)
      (all-the-icons-material
       "palette" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-lblue)))

  (defun ivy-rich-keybinding-icon (_candidate)
    "Display keybindings icons in `ivy-rich'."
    (when (display-graphic-p)
      (all-the-icons-material "keyboard" :height 1.0 :v-adjust -0.2)))

  (defun ivy-rich-library-icon (_candidate)
    "Display library icons in `ivy-rich'."
    (when (display-graphic-p)
      (all-the-icons-material
       "view_module" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-lblue)))

  (defun ivy-rich-package-icon (_candidate)
    "Display package icons in `ivy-rich'."
    (when (display-graphic-p)
      (all-the-icons-faicon
       "archive" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-silver)))

  (when (display-graphic-p)
    (defun ivy-rich-bookmark-type-plus (candidate)
      (let ((filename (file-local-name
                       (ivy-rich-bookmark-filename candidate))))
        (cond ((null filename)
               (all-the-icons-material "block" :v-adjust -0.2 :face 'warning))
              ((file-remote-p filename)
               (all-the-icons-material "wifi_tethering" :v-adjust -0.2 :face 'mode-line-buffer-id))
              ((not (file-exists-p filename))
               (all-the-icons-material
                "block" :v-adjust -0.2 :face 'error))
              ((file-directory-p filename)
               (all-the-icons-octicon
                "file-directory" :height 0.9 :v-adjust -0.05))
              (t (all-the-icons-icon-for-file
                  (file-name-nondirectory filename)
                  :height 0.9 :v-adjust -0.05)))))
    (advice-add #'ivy-rich-bookmark-type
                :override #'ivy-rich-bookmark-type-plus))
  :hook (ivy-rich-mode . (lambda ()
                           (setq ivy-virtual-abbreviate
                                 (or (and ivy-rich-mode
                                          'abbreviate) 'name))))
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-virtual-abbreviate 'full)
  (ivy-rich-switch-buffer-align-virtual-buffer t)
  (ivy-rich-path-style 'abbrev)
  :config
  (ivy-rich-mode +1))

;; (use-package ivy-historian
;;   :after ivy
;;   ;; BULK-ENSURE :ensure t
;;   :config
;;   (ivy-historian-mode +1))
(use-package ivy-hydra
  :disabled t
  :after ivy hydra
  :defer t)

(use-package ivy-rtags
  :disabled t
  :after ivy)

(use-package ivy-youtube
  :disabled t
  :after ivy)

(use-package ivy-pass
  :disabled t
  :after ivy)

(use-package ivy-todo
  :disabled t
  ;; BULK-ENSURE :ensure t
  :after ivy
  :defer t
  :bind ("C-c t" . ivy-todo)
  :commands ivy-todo)

(use-package all-the-icons-ivy
  :disabled t
  :after ivy
  :defer t
  :config
  (all-the-icons-ivy-setup))

(use-package all-the-icons-ivy-rich
  :disabled t
  :after ivy-rich
  :config (all-the-icons-ivy-rich-mode +1))

(use-package swiper
  :disabled t
  :after ivy
  :bind (("C-M-+" . swiper-mc)
         ("C-s" . swiper)
         ("C-r" . swiper))
  :config
  (defun swiper-mc ()
    (interactive)
    (unless (require 'multiple-cursors nil t)
      (error "Multiple-cursors isn't installed"))
    (let ((cands (nreverse ivy--old-cands)))
      (unless (string= ivy-text "")
        (ivy-set-action
         (lambda (_)
           (let (cand)
             (while (setq cand (pop cands))
               (swiper--action cand)
               (when cands
                 (mc/create-fake-cursor-at-point))))
           (mc/maybe-multiple-cursors-mode)))
        (setq ivy-exit 'done)
        (exit-minibuffer)))))

;;;_ , jdee

;; (use-package jdee
;;   :disabled t
;;   :config
;;   (setq jdee-server-dir "/home/emacs/jdee-server/target"))

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
  :defer t
  ;; BULK-ENSURE :ensure t
  :init
  (progn
    (defun my-js-mode-hook ()
      (abbrev-mode 1)
      (gtags-mode 1)
      (whitespace-mode 1)
      (which-function-mode 1)
      (flymake-jslint-load)
      (hs-minor-mode 1)
      (yas/minor-mode 1)

      (diminish 'gtags-mode)
      (diminish 'hs-minor-mode))
    (add-hook 'js2-mode-hook 'my-js-mode-hook)))

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
  :unless (or install-run noninteractive))

;;;_ , key-chord

(use-package key-chord
  ;; BULK-ENSURE :ensure t
  :defer t
  :commands key-chord-mode
  :init
  (key-chord-mode 1)
  :config
  (setq key-chord-two-keys-delay 0.1))

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

;;;_ , lentic-mode

(use-package lentic
  :disabled t
  :defer t
  :diminish " L"
  :config
  (global-lentic-mode 1))

;;;_ , lice

(use-package lice
  :defer t)

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

(use-package lsp-mode
  :disabled t
  :defer t
  :preface
  (setq gc-cons-threshold 100000000
        read-process-output-max (* 1024 1024))
  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  :hook ((go-mode . #'lsp-deferred)
         (go-mode . #'lsp-go-install-save-hooks)
         (java-mode . lsp)
         (python-mode . lsp)
         (scala-mode . lsp)
         (clojure-mode . lsp)
         (clojurec-mode . lsp)
         (clojurescript-mode . lsp))
  :custom
  (lsp-prefer-flymake nil)
  :config
  (lsp-register-custom-settings
   '(("gopls.completeUnimported" t t)
     ("gopls.staticcheck" t t)))
  (dolist (m '(clojure-mode
               clojurec-mode
               clojurex-mode
               clojurescript-mode
               ))
    (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))
  )

(use-package lsp-java)

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)

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
  :unless noninteractive
  :after magit)

(use-package magit-topgit
  :after magit
  ;; BULK-ENSURE :ensure t
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

(use-package modus-operandi-theme
  :disabled t)

(use-package modus-vivendi-theme
  :disabled t)

;; ;;;_ , mu4e

(use-package mu4e
  :disabled t
  ;; note to self: to update, cd /home/emacs and git fetch
  ;; :if (file-exists-p mu4e-context-file)
  :load-path "/usr/local/share/emacs/site-lisp/mu4e"
  ;; :commands (mu4e mu4e-headers-search mu4e-compose-new mu4e~proc-add)
  :preface
  (require 'auth-source t)
  (require 'org-mime)

  :secret mu4e-contexts
  :config
  (defun my/mu4e-change-headers ()
    "Adjust header sizes."
    (interactive)
    (setq mu4e-headers-fields
          `((:human-date . 25))
          ;; alternatively, use :date
          (:flags . 6)
          (:from . 22)
          (:thread-subject . ,(- (window-body-width) 70))
          ;; alternatively, use :subject
          (:size . 7)))

  (setq mu4e-maildir (expand-file-name "~/Maildir"))

                                        ; get mail
  (setq mu4e-get-mail-command "mbsync -c ~/.mbsyncrc -a"
        ;; mu4e-html2text-command "w3m -T text/html" ;;using the default mu4e-shr2text
        mu4e-view-prefer-html t
        mu4e-update-interval 180
        mu4e-headers-auto-update t
        mu4e-compose-signature-auto-include nil
        mu4e-compose-format-flowed t)

  ;; to view selected message in the browser, no signin, just html mail
  (add-to-list 'mu4e-view-actions
               '("ViewInBrowser" . mu4e-action-view-in-browser) t)

  ;; enable inline images
  (setq mu4e-view-show-images t)
  ;; use imagemagick, if available
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

  ;; every new email composition gets its own frame!
  (setq mu4e-compose-in-new-frame t)

  ;; don't save message to Sent Messages, IMAP takes care of this
  (setq mu4e-sent-messages-behavior 'delete)

  (add-hook 'mu4e-view-mode-hook #'visual-line-mode)

  ;; <tab> to navigate to links, <RET> to open them in browser
  (add-hook 'mu4e-view-mode-hook
            (lambda()
              ;; try to emulate some of the eww key-bindings
              (local-set-key (kbd "<RET>") 'mu4e~view-browse-url-from-binding)
              (local-set-key (kbd "<tab>") 'shr-next-link)
              (local-set-key (kbd "<backtab>") 'shr-previous-link)))

  ;; from https://www.reddit.com/r/emacs/comments/bfsck6/mu4e_for_dummies/elgoumx
  (add-hook 'mu4e-headers-mode-hook #'my/mu4e-change-headers)

  ;; if you use date instead of human-date in the above, use this setting
  ;; give me ISO(ish) format date-time stamps in the header list
                                        ;(setq mu4e-headers-date-format "%Y-%m-%d %H:%M")

  ;; spell check
  (add-hook 'mu4e-compose-mode-hook
            (defun my-do-compose-stuff ()
              "My settings for message composition."
              (visual-line-mode)
              (org-mu4e-compose-org-mode)
              (use-hard-newlines -1)
              (flyspell-mode)))

  (require 'smtpmail)

  ;;rename files when moving
  ;;NEEDED FOR MBSYNC
  (setq mu4e-change-filenames-when-moving t)

  ;;set up queue for offline email
  ;;use mu mkdir  ~/Maildir/acc/queue to set up first
  (setq smtpmail-queue-mail nil) ;; start in normal mode

  ;;from the info manual
  (setq mu4e-attachment-dir  "~/Downloads")

  (setq mu4e-compose-context-policy 'ask-if-none
        mu4e-context-policy         'pick-first)

  (setq message-kill-buffer-on-exit t)
  (setq mu4e-compose-dont-reply-to-self t)

  (require 'org-mu4e)

  ;; convert org mode to HTML automatically
  (setq org-mu4e-convert-to-html t)

  ;;from vxlabs config
  ;; show full addresses in view message (instead of just names)
  ;; toggle per name with M-RET
  (setq mu4e-view-show-addresses 't)

  ;; don't ask when quitting
  (setq mu4e-confirm-quit nil)

  ;; mu4e-context
  (setq mu4e-context-policy 'pick-first)
  (setq mu4e-compose-context-policy 'always-ask)

  (setq mu4e-hide-index-messages t)

  (defun mu4e-sometimes-silent-update-force ()
    "Only show minibuffer information when explicitly called."
    (interactive)
    (setq mu4e-hide-index-messages nil)
    (mu4e-maildirs-extension-force-update)
    (sleep-for 5)
    (setq mu4e-hide-index-messages t))

  (defun mu4e-front-keys ()
    "For use on mu4e main menu screen."
    (define-key mu4e-main-mode-map (kbd "u")
      'mu4e-sometimes-silent-update-force))

  (add-hook 'mu4e-main-mode-hook 'mu4e-front-keys))

;; (progn
;;   ;; (load-file mu4e-context-file)

;;   (defconst mu4e-gen-dotofflineimaprc-glue-file
;;     (concat (file-name-as-directory mu4e-gen-dotofflineimaprc-directory)
;;             "generated_offlineimap_glue.py")
;;     "Location of generated glue file.")

;;   (mu4e-gen-dotofflineimaprc-generate-glue-file mu4e-gen-dotofflineimaprc-glue-file)

;;   (defconst mu4e-gen-dotofflineimaprc-file
;;     (concat (file-name-as-directory mu4e-gen-dotofflineimaprc-directory)
;;             ".generated")
;;     "Location of generated rc file.")

;;   (mu4e-gen-dotofflineimaprc-generate-file mu4e-gen-dotofflineimaprc-file)

;;   ;; (require 'bbdb-loaddefs)
;;   (require 'starttls)
;;   (require 'org-mu4e)

;;   ;; (defun mu4e-message-maildir-matches (msg rx)
;;   ;;   (when rx
;;   ;;     (if (listp rx)
;;   ;;         ;; If rx is a list, try each one for a match
;;   ;;         (or (mu4e-message-maildir-matches msg (car rx))
;;   ;;             (mu4e-message-maildir-matches msg (cdr rx)))
;;   ;;       ;; Not a list, check rx
;;   ;;       (string-match rx (mu4e-message-field msg :maildir)))))

;;   (defun mu4e-find-context-from-mail-address (address)
;;     "Given canonical email ADDRESS in 'User Name <acount@domain.tld>'
;;        form, return name of context or nil."
;;     (message "Finding context from mail address %s" address)
;;     (cdr (assoc address
;;                 (cl-mapcar
;;                  #'(lambda (x) (cons (concat
;;                                  (alist-get 'user-full-name
;;                                             (mu4e-context-vars x))
;;                                  " <"
;;                                  (alist-get 'user-mail-address
;;                                             (mu4e-context-vars x))
;;                                  ">")

;;                                 (mu4e-context-name x))  )
;;                  mu4e-contexts))))

;;   (defun choose-msmtp-account ()
;;     (if (message-mail-p)
;;         (save-excursion
;;           (let*
;;               ((from-addr (save-restriction (message-narrow-to-headers)
;;                                             (message-fetch-field "from")))
;;                (account (mu4e-find-context-from-mail-address from-addr)
;;                         ))
;;             (message "Chose %s" account)
;;             (setq message-sendmail-extra-arguments (list '"-a" account))))))

;;   ;; Arrange to view messages in either the default browser or EWW
;;   (add-to-list 'mu4e-view-actions
;;                '("ViewInBrowser" . mu4e-action-view-in-browser) t)
;;   (add-to-list 'mu4e-view-actions '("Eww view" . jcs-view-in-eww) t)

;;   ;; From Ben Maughan: Get some Org functionality in compose buffer
;;   (add-hook 'message-mode-hook 'turn-on-orgtbl)
;;   (add-hook 'message-mode-hook 'turn-on-orgstruct++)

;;   ;; (setq mu4e-headers-fields
;;   ;;       '( (:date          .  25)    ;; alternatively, use :human-date
;;   ;;          75:        (:flags         .   6)
;;   ;;          76:        (:from          .  22)
;;   ;;          77:        (:subject       .  nil)))
;;   (setq mu4e-maildir "~/Maildir")
;;   (setq org-mu4e-link-query-in-headers-mode nil)
;;   (setq mu4e-compose-format-flowed t)
;;   (setq mu4e-update-interval 300)
;;   (setq mu4e-view-show-images t)
;;   (setq mu4e-html2text-command "w3m -dump -T text/html")
;;   (setq mu4e-headers-include-related t)
;;   (setq mu4e-attachment-dir  "~/Downloads")
;;   (setq mu4e-headers-date-format "%Y-%m-%d %H:%M")
;;   (setq mu4e-view-show-addresses 't)
;;   (setq message-kill-buffer-on-exit t)
;;   ;; This prevents saving the email to the Sent folder since gmail will do
;;   ;; this for us on their end.
;;   (setq mu4e-sent-messages-behavior 'delete)
;;   (setq message-kill-buffer-on-exit t)
;;   ;; Use imagemagick, if available.
;;   (when (fboundp 'imagemagick-register-types)
;;     (imagemagick-register-types))

;;   ;; Sometimes html email is just not readable in a text based client, this
;;   ;; lets me open the email in my browser.
;;   (add-to-list 'mu4e-view-actions
;;                '("View in browser" . mu4e-action-view-in-browser) t)

;;   ;; (add-hook
;;   ;;  'mu4e-mark-execute-pre-hook
;;   ;;  (lambda (mark msg)
;;   ;;    (cond
;;   ;;     ((member mark
;;   ;;              '(refile trash)) (mu4e-action-retag-message msg "-\\Inbox"))
;;   ;;     ((equal mark 'flag) (mu4e-action-retag-message msg "\\Starred"))
;;   ;;     ((equal mark 'unflag) (mu4e-action-retag-message msg "-\\Starred")))))

;;   ;; Configure sending mail.
;;   (setq message-send-mail-function 'message-send-mail-with-sendmail
;;         sendmail-program (executable-find "msmtp"))

;;   ;; Use the correct account context when sending mail based on the from
;;   ;; header.
;;   (setq message-sendmail-envelope-from 'header)
;;   (add-hook 'message-send-mail-hook 'choose-msmtp-account)

;;   (setq message-send-mail-function 'message-send-mail-with-sendmail
;;         sendmail-program "msmtp"
;;         )

;;   ;; Use the correct account context when sending mail based on the from header.
;;   (setq message-sendmail-envelope-from 'header)
;;   (add-hook 'message-send-mail-hook 'choose-msmtp-account)

;;   (setq mu4e-get-mail-command
;;         (mapconcat #'identity
;;                    (list "offlineimap"
;;                          (when mu4e-gen-dotofflineimaprc-glue-file
;;                            (concat "-c "
;;                                    mu4e-gen-dotofflineimaprc-glue-file)))
;;                    " ")

;;         mu4e-attachment-dir (ensure-directory "~/Downloads")
;;         mu4e-context-policy 'pick-first

;;         mu4e-change-filenames-when-moving t
;;         mu4e-compose-dont-reply-to-self t
;;         message-kill-buffer-on-exit t
;;         mu4e-action-tag-headers "X-Keywords"

;;         mu4e-view-show-addresses t)

;;   ;; (setq starttls-use-gnutls t)

;;   ;; (require 'smtpmail)

;;   ;; (setq send-mail-function            'smtpmail-send-it
;;   ;;       message-send-mail-function    'smtpmail-send-it
;;   ;;       smtpmail-auth-credentials     (expand-file-name "~/.authinfo.gpg")
;;   ;;       smtpmail-stream-type          'tls
;;   ;;       smtpmail-smtp-server          "smtp.gmail.com"
;;   ;;       smtpmail-smtp-service         465)

;;   (setq  mu4e-good-filter
;;          (concat " AND NOT "
;;                  (string-join
;;                   (cl-mapcar
;;                    #'(lambda (x) (concat "'m:"
;;                                     (alist-get 'mu4e-trash-folder
;;                                                (mu4e-context-vars x))
;;                                     "'"))
;;                    mu4e-contexts)
;;                   " OR ")
;;                  " ")

;;          mu4e-inbox-filter-base
;;          (concat " "
;;                  (string-join
;;                   (cl-mapcar
;;                    #'(lambda (x) (concat "'m:"
;;                                     (alist-get 'mu4e-inbox-folder
;;                                                (mu4e-context-vars x))
;;                                     "'"))
;;                    mu4e-contexts)
;;                   " OR ")
;;                  " ")

;;          mu4e-unread-filter
;;          " ( flag:unread AND NOT flag:flagged AND NOT flag:trashed ) "

;;          mu4e-unread-flagged-filter
;;          " ( flag:unread AND flag:flagged AND NOT flag:trashed ) "

;;          mu4e-bookmarks
;;          (append (list
;;                   (list (concat "flag:unread AND NOT flag:trashed AND "
;;                                 mu4e-inbox-filter-base)
;;                         "Unread [i]NBOX messages" ?i)

;;                   (list (concat "flag:flagged AND NOT flag:trashed AND "
;;                                 mu4e-inbox-filter-base)
;;                         "[f]lagged INBOX messages" ?f)

;;                   (list (concat mu4e-unread-filter
;;                                 mu4e-good-filter)
;;                         "Unread messages" ?a)

;;                   (list (concat mu4e-unread-flagged-filter
;;                                 mu4e-good-filter)
;;                         "Unread-flagged messages" ?A)
;;                   )
;;                  (mapcar (lambda (x)
;;                            (cons (concat
;;                                   (car x)
;;                                   mu4e-good-filter) (cdr x)))

;;                          '(("flag:unread AND NOT flag:trashed"
;;                             "Unread messages" ?u)

;;                            ("date:1h..now"
;;                             "Last hours messages" ?h)
;;                            ("date:24h..now"
;;                             "Today's messages" ?d)
;;                            ("date:today..now"
;;                             "Today's messages" ?t)
;;                            ("date:7d..now"
;;                             "Last 7 days" ?w)
;;                            ("mime:*pdf"
;;                             "Messages with PDF" 112)
;;                            ("mime:*vcs"
;;                             "Messages with VCS" 113)
;;                            )))

;;          bbdb-mail-user-agent (quote mu4e-user-agent)
;;          mu4e-view-mode-hook (quote (visual-line-mode))
;;          mu4e-compose-complete-addresses nil
;;          bbdb-mua-pop-up t
;;          bbdb-mua-pop-up-window-size 5
;;          )


;;   )


(use-package mu4e-alert
  :after mu4e
  :init
  (setq mu4e-alert-interesting-mail-query
        (concat
         "flag:unread "
         "AND NOT "
         "flag:trashed"))
  :config
  (mu4e-alert-enable-mode-line-display)

  (defun gjstein-refresh-mu4e-alert-mode-line ()
    (interactive)
    (mu4e~proc-kill)
    (mu4e-alert-enable-mode-line-display))

  (run-with-timer 0 60 'gjstein-refresh-mu4e-alert-mode-line))

(use-package mu4e-conversation
  :after mu4e
  :config
  (global-mu4e-conversation-mode))

(use-package mu4e-jump-to-list
  :after mu4e)

(use-package mu4e-maildirs-extension
  :after mu4e
  :config
  (mu4e-maildirs-extension))

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
  :defer t
  :bind (("C-c t" . multi-term-next)
         ("C-c T" . multi-term))
  :init
  (defun screen ()
    (interactive)
    (let (term-buffer)
      ;; Set buffer.
      (setq term-buffer
            (let ((multi-term-program (executable-find "screen"))
                  (multi-term-program-switches "-DR"))
              (multi-term-get-buffer)))
      (set-buffer term-buffer)
      ;; Internal handle for `multi-term' buffer.
      (multi-term-internal)
      ;; Switch buffer
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

;;;_ , multiple-cursors

(use-package multiple-cursors
  :after selected
  :defer 5

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

(use-package multiple-cursors
  :defer t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-S-<mouse-1>" . mc/toggle-cursor-on-click)
         ("s-l" . mc/edit-lines)
         ("C-+" . mc/mark-more-like-this-extended)
         ("C-c C-l". mc/mark-all-like-this)
         ("s-e" . mc/edit-ends-of-lines)
         ("s-a" . mc/edit-beginnings-of-lines)
         ("s-C-d" . mc/mark-all-like-this)))

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
;;;_ , muse

;; (use-package muse
;;   :mode ("\\.muse\\'" . muse-mode)
;;   :init
;;   ((setq muse-project-alist
;;          `(("Website" ("~/Pages" :default "index")
;;             (:base "html" :path "~/public_html"))
;;            ;;   1. Source directory
;;            ;;   2. Output directory
;;            ;;   3. Publishing style
;;            ;;   remainder: Other things to put in every generated style
;;            ,@(muse-project-alist-styles "~/Blog"
;;                                         "~/public_html/blog"
;;                                         "blosxom")))))

;;;_ , nf-procmail-mode

;; (use-package nf-procmail-mode
;;   :commands nf-procmail-mode)

(use-package native-complete
  :after shell
  :config
  (native-complete-setup-bash))

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
  :custom (completion-styles '(orderless)))

;;;_ , org-mode

(use-package my-dot-org
  :straight nil
  :preface
  (load dot-org)
  :commands my-org-startup
  :bind* (("M-C"   . jump-to-org-agenda)
          ("M-m"   . org-smart-capture)
          ("M-M"   . org-inline-note)
          ("C-c a" . org-agenda)
          ("C-c S" . org-store-link)
          ("C-c l" . org-insert-link)))

;; (key-chord-define-global
;;  "hh"
;;  (defhydra my/key-chord-commands ()
;;    "Main"
;;    ;; ...
;;    ("L" my/org-insert-link)
;;    ("l" org-insert-last-stored-link)
;;    ;; ...
;;    )
;;  )

;; org prettify


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
  :defer t
  :config
  (outshine-mode +1)
  (use-package navi-mode))

;;;_ , pabbrev

(use-package pabbrev
  :commands pabbrev-mode
  :diminish pabbrev-mode)

;;;_ ; paradox
(use-package paradox
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
    (when (el-patch-swap (and package-menu-async package--downloads-in-progress)
                         (and package-menu-async package--downloads-in-progress
                              (seq-difference package--downloads-in-progress '(paradox--data))))
      (user-error "Package refresh is already in progress, please wait..."))
    (setq package-menu--old-archive-contents package-archive-contents)
    (setq package-menu--new-package-list nil)
    (package-refresh-contents package-menu-async))
  :config
  (paradox-enable))

;;;_ , paredit

(use-package paredit
  :unless noninteractive
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

(use-package paredit-everywhere
  :diminish paredit-everywhere-mode
  :config
  (add-hook 'prog-mode-hook 'paredit-everywhere-mode))

;;;_ , paren

(use-package mic-paren
  :config
  (paren-activate))

(use-package paren
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
  :commands (pass pass-view-mode)
  :mode ("\\.passwords/.*\\.gpg\\'" . pass-view-mode)
  :hook (pass-view-mode . #'pass-view--prepare-otp))

(use-package password-store
  :defer 5
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

;;;_ , persp-mode

(use-package persp-mode
  :disabled t
  :unless noninteractive
  ;; :commands (
  ;;            persp-switch
  ;;            persp-rename
  ;;            persp-temporarily-display-buffer
  ;;            persp-add-buffer
  ;;            persp-remove-buffer
  ;;            persp-prev
  ;;            persp-next
  ;;            persp-save-state-to-file
  ;;            persp-load-state-from-file
  ;;            )
  :config
  (progn
    (setq wg-morph-on nil
          persp-keymap-prefix (kbd "C-,")
          persp-auto-save-opt 2
          persp-auto-save-num-of-backups 3
          persp-save-dir (ensure-directory "~/.emacs.d/persp-confs")
          persp-auto-resume-time 0
          persp-set-last-persp-for-new-frames nil)

    (with-eval-after-load "ivy"
      (add-hook 'ivy-ignore-buffers
                #'(lambda (b)
                    (when persp-mode
                      (let ((persp (get-current-persp)))
                        (if persp
                            (not (persp-contain-buffer-p b persp))
                          nil)))))

      (setq ivy-sort-functions-alist
            (append ivy-sort-functions-alist
                    '((persp-kill-buffer   . nil)
                      (persp-remove-buffer . nil)
                      (persp-add-buffer    . nil)
                      (persp-switch        . nil)
                      (persp-window-switch . nil)
                      (persp-frame-switch  . nil)))))

    (use-package persp-mode-projectile-bridge
      :disabled t
      ;; BULK-ENSURE :ensure t
      :config
      (add-hook
       'persp-mode-projectile-bridge-mode-hook
       #'(lambda ()
           (if persp-mode-projectile-bridge-mode
               (persp-mode-projectile-bridge-find-perspectives-for-all-buffers)
             (persp-mode-projectile-bridge-kill-perspectives))))
      (add-hook
       'after-init-hook
       #'(lambda ()
           (persp-mode-projectile-bridge-mode 1))
       t))

    (with-eval-after-load "erc"
      (persp-def-auto-persp "erc"
                            :mode 'erc-mode
                            :dyn-env '(after-switch-to-buffer-functions
                                       ;; prevent recursion
                                       (persp-add-buffer-on-find-file nil)
                                       persp-add-buffer-on-after-change-major-mode)
                            :switch 'frame))


    (with-eval-after-load "mu4e"
      (persp-def-auto-persp "mu4e"
                            :buffer-name "^\*mu4e.+\*"))

    ;; (with-eval-after-load "erc"
    ;;   (persp-def-buffer-save/load
    ;;    :mode 'erc-mode :tag-symbol 'def-erc-server
    ;;    :save-vars '("^erc-session-.+" "^erc-server-.+")
    ;;    :save-function
    ;;    #'(lambda (buffer tag lvars)
    ;;        (if (get-buffer-process buffer)
    ;;            (progn
    ;;              (push (cons 'persp-erc-chans
    ;;                          (mapcar #'buffer-name
    ;;                                  (erc-channel-list
    ;;                                   (get-buffer-process buffer))))
    ;;                    lvars)
    ;;              (push (cons 'persp-erc-persp-name
    ;;                          (car (buffer-local-value
    ;;                                'persp-buffer-in-persps
    ;;                                buffer)))
    ;;                    lvars)
    ;;              (list tag (buffer-name buffer) lvars))
    ;;          'skip))
    ;;    :after-load-function
    ;;    #'(lambda (erc-buf &rest _other)
    ;;        (lexical-let
    ;;            (chans
    ;;             erc-persp-name erc-persp (erc-buf erc-buf) initial-persp
    ;;             erc-window
    ;;             persp-erc-after-connect-lambda persp-erc-join-lambda)
    ;;          (setq persp-erc-after-connect-lambda
    ;;                #'(lambda (ntwrk nck)
    ;;                    (setq erc-window (selected-window))
    ;;                    (set-window-buffer erc-window erc-buf)
    ;;                    (add-hook 'erc-server-JOIN-functions
    ;;                              persp-erc-join-lambda
    ;;                              t)
    ;;                    (mapc #'(lambda (chan)
    ;;                              (with-current-buffer erc-buf
    ;;                                (persp-add-buffer (erc-join-channel chan nil)
    ;;                                                  erc-persp)))
    ;;                          chans)
    ;;                    (remove-hook 'erc-after-connect
    ;;                                 persp-erc-after-connect-lambda)
    ;;                    nil)
    ;;                persp-erc-join-lambda
    ;;                #'(lambda (proc parsed)
    ;;                    (if chans
    ;;                        (when (eq proc (get-buffer-process erc-buf))
    ;;                          (let ((chan (erc-response.contents parsed)))
    ;;                            (when (member chan chans)
    ;;                              (setq chans (delete chan chans))
    ;;                              (when erc-persp
    ;;                                (persp-add-buffer chan erc-persp))
    ;;                              (unless chans
    ;;                                (remove-hook 'erc-server-JOIN-functions
    ;;                                             persp-erc-join-lambda)
    ;;                                ;; (persp-frame-switch
    ;;                                ;;  (safe-persp-name initial-persp))
    ;;                                ))))
    ;;                      (remove-hook 'erc-server-JOIN-functions
    ;;                                   persp-erc-join-lambda))
    ;;                    nil))
    ;;          (with-current-buffer erc-buf
    ;;            (setq chans persp-erc-chans
    ;;                  erc-persp-name persp-erc-persp-name))
    ;;          (when erc-persp-name
    ;;            (setq erc-persp (persp-get-by-name erc-persp-name))
    ;;            (setq initial-persp (get-current-persp))
    ;;            (persp-frame-switch erc-persp-name))
    ;;          (setq erc-window (get-buffer-window erc-buf (selected-frame)))
    ;;          (if (window-live-p erc-window)
    ;;              (select-window erc-window)
    ;;            (setq erc-window (selected-window))
    ;;            (set-window-buffer erc-window erc-buf))
    ;;          (add-hook 'erc-after-connect persp-erc-after-connect-lambda t)
    ;;          (with-current-buffer erc-buf
    ;;            (erc-server-reconnect)
    ;;            (persp-special-last-buffer-make-current))))))

    (add-hook 'kill-emacs-hook 'persp-save-state-to-file)
    (add-hook 'after-init-hook
              #'(lambda ()
                  (message "persp-mode on")
                  (persp-mode 1)
                  (persp-load-state-from-file persp-auto-save-fname)))))

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

(use-package perspective
  :disabled t
  :unless noninteractive
  :preface
  (defun make-variable-frame-local (v)
    "Frame-locals no longer supported but perspective wants V."
    (message "fake frame local variable created"))
  :config
  (progn
    (defvar persp-shared-buffers '("*scratch*" "*Messages*" "*Backtrace*"))
    (add-hook 'persp-activated-functions
              #'(lambda (_)
                  (persp-add-buffer persp-shared-buffers)))

    (set-face-attribute 'persp-selected-face nil :foreground "#81a2be")

    (defmacro custom-persp (name &rest body)
      `(let ((initialize (not (gethash ,name perspectives-hash)))
             (current-perspective persp-curr))
         (persp-switch ,name)
         (when initialize ,@body)
         (setq persp-last current-perspective)))

    (add-hook 'after-init-hook
              #'(lambda ()
                  (message "persp-mode on")
                  (persp-mode 1)
                  (persp-switch "main")))))

(use-package persp-projectile
  ;; :load-path "/home/emacs/persp-projectile/"
  :after perspective projectile
  :config
  (add-hook 'persp-activated-hook
            #'(lambda ()
                (persp-add-buffer
                 (get-buffer-create "*Messages*"))))

  (setq projectile-mode-line
        '(:eval (if (file-remote-p default-directory)
                    " Prj[*remote*]"
                  (format " Prj[%s]" (projectile-project-name))))))

(use-package plantuml-mode
  :mode "\\.plantuml\\'"
  :custom
  (plantuml-default-exec-mode 'jar)
  (plantuml-jar-path "/usr/share/java/plantuml.jar"))

(use-package poporg
  :bind ("C-x C-;" . poporg-dwim))

;;;_ , powerline

(use-package powerline
  :disabled t
  :config
  (powerline-default-theme))

;;;_ , pp-c-l

(use-package pp-c-l
  :disabled t
  :hook (prog-mode . #'pretty-control-l-mode))

;;;_ , predictive-mode
;; company-statistics seems good enough?

;; (use-package predictive)

(use-package prescient)

(use-package prism
  :disabled t
  :hook ((lisp-mode clojure-mode json-mode) . prism-mode)
  :hook ((lisp-mode python-mode) . prism-whitespace-mode))

;;;_ , projectile

(use-package project
  :config
  (setq project-switch-commands (list)))

(use-package projectile
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
  :config
  (progn
    (defun ps-spool-to-pdf (beg end &rest ignore)
      (interactive "r")
      (let ((temp-file (concat (make-temp-name "ps2pdf") ".pdf")))
        (call-process-region beg end (executable-find "ps2pdf")
                             nil nil nil "-" temp-file)
        (call-process (executable-find "open") nil nil nil temp-file)))

    (setq ps-print-region-function 'ps-spool-to-pdf)))

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

(use-package puppet-mode
  :config
  (progn
    ;; (with-eval-after-load 'align
    ;;  (push '(ruby-arrow
    ;;          (regexp   . "\\(\\s-*\\)=>\\(\\s-*\\)")
    ;;          (group    . (1 2))
    ;;          (modes    . '(ruby-mode puppet-mode)))
    ;;        'align-rules-list))

    ;; (defvar puppet-anchor-point nil)

    ;; (defun puppet-set-anchor ()
    ;;   (interactive)
    ;;   (setq puppet-anchor-point (point-marker))
    ;;   (message "puppet-mode anchor set at %s"
    ;;            (marker-position puppet-anchor-point)))

    ;; (defun puppet-resource-beginning ()
    ;;   (save-excursion
    ;;     (and (re-search-backward
    ;;           "^\\s-*\\(\\S-+\\)\\s-+{\\s-+\\([^:]+\\):" nil t)
    ;;          (list (match-beginning 0)
    ;;                (match-string 1) (match-string 2)))))

    ;; (defun puppet-resource-end ()
    ;;   (save-excursion
    ;;     (and (re-search-forward "^\\s-*}" nil t)
    ;;          (match-end 0))))

    ;; (defun puppet-create-require ()
    ;;   (interactive)
    ;;   (require 'align)
    ;;   (if (null puppet-anchor-point)
    ;;       (error "Anchor point has not been set")
    ;;     (destructuring-bind (anchored-start resource name)
    ;;         (save-excursion
    ;;           (goto-char puppet-anchor-point)
    ;;           (puppet-resource-beginning))
    ;;       (save-excursion
    ;;         (let ((beginning (car (puppet-resource-beginning)))
    ;;               (end (puppet-resource-end)))
    ;;           (goto-char end)
    ;;           (backward-char)
    ;;           (let ((current-requires
    ;;                  (when (re-search-backward
    ;;                         "^\\s-*require\\s-*=>\\s-*" beginning t)
    ;;                    (let ((start (match-beginning 0))
    ;;                          (beg (match-end 0)))
    ;;                      (if (looking-at "\\[")
    ;;                          (forward-sexp))
    ;;                      (re-search-forward "\\([,;]\\)?[ \t]*\n")
    ;;                      (prog1
    ;;                          (buffer-substring-no-properties
    ;;                           beg (match-beginning 0))
    ;;                        (delete-region start (point)))))))
    ;;             (save-excursion
    ;;               (skip-chars-backward " \t\n\r")
    ;;               (when (looking-back ";")
    ;;                 (delete-backward-char 1)
    ;;                 (insert ?,)))
    ;;             (insert "  require => ")
    ;;             (if current-requires
    ;;                 (insert "[ " current-requires ", "))
    ;;             (insert (capitalize (substring resource 0 1))
    ;;                     (substring resource 1) "[" name "]")
    ;;             (if current-requires
    ;;                 (insert " ]"))
    ;;             (insert ";\n")
    ;;             (mark-paragraph)
    ;;             (align-code (region-beginning) (region-end))))))))

    ;; (define-key puppet-mode-map [(control ?x) ? ] 'puppet-set-anchor)
    ;; (define-key puppet-mode-map [(control ?x) space] 'puppet-set-anchor)
    ;; (define-key puppet-mode-map [(control ?c) (control ?r)] 'puppet-create-require)

    (bind-key "C-x SPC" 'puppet-set-anchor puppet-mode-map)
    (bind-key "C-c C-r" 'puppet-create-require puppet-mode-map)))

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
      (unbind-key "C-c c" python-mode-map)

      (use-package company-jedi
        :after (company python)
        :defer t
        :config
        (progn
          (add-to-list 'company-backends 'company-jedi))))
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
  :unless noninteractive
  :hook (dired-mode . recentf-add-dired-directory)
  :commands (recentf-mode
             recentf-add-file
             recentf-apply-filename-handlers)
  :preface
  (defun recentf-add-dired-directory ()
    (if (and dired-directory
             (file-directory-p dired-directory)
             (not (string= "/" dired-directory)))
        (let ((last-idx (1- (length dired-directory))))
          (recentf-add-file
           (if (= ?/ (aref dired-directory last-idx))
               (substring dired-directory 0 last-idx)
             dired-directory)))))
  :custom
  (recentf-max-saved-items 500)
  (recentf-max-menu-items 15)
  ;; disable recentf-cleanup on Emacs start, because it can cause
  ;; problems with remote files
  (recentf-auto-cleanup 'never)
  :config
  (recentf-mode +1)



  (add-hook 'dired-mode-hook 'recentf-add-dired-directory))

(use-package repl-toggle
  :custom
  (rtog/mode-repl-alist
   '((emacs-lisp-mode . ielm)
     (ruby-mode . inf-ruby)
     (js-mode . nodejs-repl)
     (typescript-mode . run-ts)))
  :config
  (repl-toggle-mode))

;;;_ , redshank

(use-package redshank
  :disabled t
  :diminish redshank)

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
  :init
  (setq savehist-additional-variables
        ;; search entries
        '(search-ring regexp-search-ring extended-command-history)
        ;; save every minute
        savehist-autosave-interval 60
        ;; keep the home clean
        savehist-file (locate-user-emacs-file "savehist"))
  :config
  (savehist-mode +1))

;;;_ , sed-mode

(use-package sed-mode)

;;;_ , select-themes

(use-package select-themes)

;;;_ , selected

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

;;;_ , session

(use-package session
  :disabled t ;; in favor of desktop-save-mode
  :unless noninteractive
  :config
  (progn
    (session-initialize)

    (defun remove-session-use-package-from-settings ()
      (when (string= (buffer-file-name)
                     (locate-user-emacs-file "settings.el"))
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward "^ '(session-use-package " nil t)
            (delete-region (line-beginning-position)
                           (1+ (line-end-position)))))))

    (add-hook 'before-save-hook 'remove-session-use-package-from-settings)

    ;; expanded folded secitons as required
    (defun le::maybe-reveal ()
      (when (and (or (memq major-mode  '(org-mode outline-mode))
                     (and (boundp 'outline-minor-mode)
                          outline-minor-mode))
                 (outline-invisible-p))
        (if (eq major-mode 'org-mode)
            (org-reveal)
          (show-subtree))))

    (add-hook 'session-after-jump-to-last-change-hook
              'le::maybe-reveal)

    (defun save-information ()
      (with-temp-message "Saving Emacs information..."
        (recentf-cleanup)

        (loop for func in kill-emacs-hook
              unless (memq func '(exit-gnus-on-exit server-force-stop))
              do (funcall func))))

    (run-with-idle-timer 300 t 'save-information)

    (if window-system
        (add-hook 'after-init-hook 'session-initialize t))))

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


;; (use-package slime
;;   ;; BULK-ENSURE :ensure t
;;   :defer t
;;   :commands (sbcl slime)
;;   :init
;;   (add-hook
;;    'slime-load-hook
;;    #'(lambda ()
;;        (slime-setup
;;         '(slime-asdf
;;           slime-autodoc
;;           slime-banner
;;           slime-c-p-c
;;           slime-company
;;           slime-editing-commands
;;           slime-fancy-inspector
;;           slime-fancy
;;           slime-fuzzy
;;           slime-highlight-edits
;;           slime-parse
;;           slime-presentation-streams
;;           slime-presentations
;;           slime-references
;;           slime-repl
;;           slime-sbcl-exts
;;           slime-package-fu
;;           slime-fontifying-fu
;;           slime-mdot-fu
;;           slime-scratch
;;           slime-tramp
;;           ;; slime-enclosing-context
;;           ;; slime-typeout-frame
;;           slime-xref-browser))

;;        (define-key slime-repl-mode-map [(control return)] 'other-window)

;;        (define-key slime-mode-map [return] 'paredit-newline)
;;        (define-key slime-mode-map [(control ?h) ?F] 'info-lookup-symbol)))

;;   :config
;;   (progn
;;     (use-package slime-company
;;       ;; BULK-ENSURE :ensure t
;;       )
;;     (eval-when-compile
;;       (defvar slime-repl-mode-map))

;;     (setq slime-net-coding-system 'utf-8-unix)

;;     (setq slime-lisp-implementations
;;           '((sbcl
;;              ("sbcl" "--core"
;;               "/Users/johnw/Library/Lisp/sbcl.core-with-slime-X86-64")
;;              :init
;;              (lambda (port-file _)
;;                (format "(swank:start-server %S)\n" port-file)))
;;             (ecl ("ecl" "-load" "/Users/johnw/Library/Lisp/init.lisp"))
;;             (clisp ("clisp" "-i" "/Users/johnw/Library/Lisp/lwinit.lisp"))))

;;     (setq slime-default-lisp 'sbcl)
;;     (setq slime-complete-symbol*-fancy t)
;;     (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)

;;     (defun sbcl (&optional arg)
;;       (interactive "P")
;;       (let ((slime-default-lisp (if arg 'sbcl64 'sbcl))
;;             (current-prefix-arg nil))
;;         (slime)))
;;     (defun clisp () (interactive) (let ((slime-default-lisp 'clisp)) (slime)))
;;     (defun ecl () (interactive) (let ((slime-default-lisp 'ecl)) (slime)))

;;     (defun start-slime ()
;;       (interactive)
;;       (unless (slime-connected-p)
;;         (save-excursion (slime))))

;;     ;; (add-hook 'slime-mode-hook 'start-slime)
;;     (add-hook 'slime-load-hook #'(lambda () (require 'slime-fancy)))
;;     (add-hook 'inferior-lisp-mode-hook #'(lambda () (inferior-slime-mode t)))

;;     ))


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

;;;_ , smart-mode-line

(use-package smart-mode-line
  :disabled t
  :config
  (sml/setup)
  (sml/apply-theme 'dark))

(use-package smart-mode-line-powerline-theme
  :disabled t
  :after powerline
  :after smart-mode-line
  :config
  (sml/setup)
  (sml/apply-theme 'powerline))

;;;_ , solaire-mode

(use-package solaire-mode
  :config
  (solaire-mode)
  (add-hook 'after-change-major-mode-hook 'turn-on-solaire-mode))

;;;_ , solarized-theme

(use-package solarized-theme
  :disabled t
  :init
  (setq solarized-distinct-fringe-background t
        solarized-use-variable-pitch nil
        solarized-high-contrast-mode-line t
        x-underline-at-descent-line t)
  :config
  (progn
    ;; (load-theme 'solarized-light t)
    (load-theme 'solarized-dark t)))

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

;;;_ , sunrise-commander

(use-package sunrise-commander
  :disabled t
  :commands (sunrise sunrise-cd)
  :init
  (progn
    (defun my-activate-sunrise ()
      (interactive)
      (let ((sunrise-exists
             (loop for buf in (buffer-list)
                   when (string-match " (Sunrise)$" (buffer-name buf))
                   return buf)))
        (if sunrise-exists
            (call-interactively 'sunrise)
          (sunrise "~/dl/" "~/Archives/"))))

    (bind-key "C-c j" 'my-activate-sunrise)
    (bind-key "C-c C-j" 'sunrise-cd))

  :config
  (progn
    (require 'sunrise-x-modeline)
    (require 'sunrise-x-tree)
    (require 'sunrise-x-tabs)

    (bind-key "/" 'sr-sticky-isearch-forward sr-mode-map)
    (bind-key "<backspace>" 'sr-scroll-quick-view-down sr-mode-map)
    (bind-key "C-x t" 'sr-toggle-truncate-lines sr-mode-map)

    (bind-key "q" 'sr-history-prev sr-mode-map)
    (bind-key "z" 'sr-quit sr-mode-map)

    (unbind-key "C-e" sr-mode-map)
    (unbind-key "C-p" sr-tabs-mode-map)
    (unbind-key "C-n" sr-tabs-mode-map)
    (unbind-key "M-<backspace>" sr-term-line-minor-mode-map)

    (bind-key "M-[" 'sr-tabs-prev sr-tabs-mode-map)
    (bind-key "M-]" 'sr-tabs-next sr-tabs-mode-map)

    (defun sr-browse-file (&optional file)
      "Display the selected file with the default appication."
      (interactive)
      (setq file (or file (dired-get-filename)))
      (save-selected-window
        (sr-select-viewer-window)
        (let ((buff (current-buffer))
              (fname (if (file-directory-p file)
                         file
                       (file-name-nondirectory file)))
              (app (cond
                    ((eq system-type 'darwin)       "open %s")
                    ((eq system-type 'windows-nt)   "open %s")
                    (t                              "xdg-open %s"))))
          (start-process-shell-command "open" nil (format app file))
          (unless (eq buff (current-buffer))
            (sr-scrollable-viewer (current-buffer)))
          (message "Opening \"%s\" ..." fname))))

    (defun sr-goto-dir (dir)
      "Change the current directory in the active pane to the given one."
      (interactive (list (progn
                           (require 'lusty-explorer)
                           (lusty-read-directory))))
      (if sr-goto-dir-function
          (funcall sr-goto-dir-function dir)
        (unless (and (eq major-mode 'sr-mode)
                     (sr-equal-dirs dir default-directory))
          (if (and sr-avfs-root
                   (null (posix-string-match "#" dir)))
              (setq dir (replace-regexp-in-string
                         (expand-file-name sr-avfs-root) "" dir)))
          (sr-save-aspect
           (sr-within dir (sr-alternate-buffer (dired dir))))
          (sr-history-push default-directory)
          (sr-beginning-of-buffer))))))

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
  (add-hook 'html-mode-hook (lambda () (tagedit-mode 1))))

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

;;;_ , todochiku

;; (use-package todochiku)

;;;_ , tramp

(use-package tramp
  :config
  (setq tramp-default-method "scp"))

;;;_ , uniquify

(use-package uniquify
  :straight nil
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  ;; rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)
  ;; don't muck with special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

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
  )

;;;_ , w3m

(use-package w3m
  :commands (w3m-browse-url w3m-find-file))

;;;_ , wee-chat

(use-package weechat
  :disabled t
  :defer 1
  :custom
  (weechat-auto-monitor-buffers t)
  (weechat-auto-close-buffers t)
  (weechat-auto-monitor-new-buffers 'silent)

  :config
  (defvar weechat/match-line-regex
    "^\\([0-9]+:[0-9]+:[0-9]+\\)\s\\([^\s]*\\):\s*\\(.*\\)")

  (defvar weechat/ignore-users '("kotfic"))

  ;; Get the last line of text from a buffer
  (defun buffer/last-line (buffer &optional num)
    (or num (setq num 1))
    (save-excursion
      (set-buffer buffer)
      (save-excursion
        (end-of-buffer)
        (forward-line (- 1 num))
        (backward-char)
        (let ((end (point)))
          (forward-line 0)
          (buffer-substring-no-properties (point) end)))))

  ;; Parse a weechat line into a structured p-list
  (defun weechat/parse-line (msg)
    (when (s-matches? weechat/match-line-regex msg)
      (let ((fields '(:raw :time :user :message))
            (values (s-match weechat/match-line-regex msg)))
        (apply #'append (mapcar* (lambda (a b) (list a b)) fields values)))))

  ;; What to do if
  (defun weechat/sauron-action (plst)
    (sauron-switch-to-marker-or-buffer (plist-get plst :marker)))


  (setq
   ;; Only add event if no events for last channel-insensitivity amount of time
   weechat/channel-insensitivity 60
   ;; Hash of last message times for each channel (by :short_name)
   weechat/channel-event-hash (make-hash-table :size 100 :test 'equal))


  ;; Return true if there has been no activity since
  ;; weechat/channel-insensitivity
  (defun weechat/fresh-channel-event (channel)
    ;; we only store the lsb, which is good enough for 2^16 seconds.
    (let* ((now-lsb (float-time))
           (tstamp (gethash channel weechat/channel-event-hash)))

      ;; Always update channel hash with most recent event
      (puthash channel now-lsb weechat/channel-event-hash)

      (cond ((not tstamp) t)
            ((> (- now-lsb tstamp) weechat/channel-insensitivity) t)
            (t nil))))

  ;; Add an event at priority 3
  (defun weechat/sauron-add-event (msg prio)
    (when (not (member (plist-get msg :user) weechat/ignore-users))
      (let ((jump-pos (save-window-excursion
                        (switch-to-buffer (plist-get msg :emacs/buffer))
                        (point-max-marker))))
        (sauron-add-event 'weechat prio
                          (format "[%s] %s: %s"
                                  (plist-get msg :short_name)
                                  (plist-get msg :user)
                                  (plist-get msg :message))
                          (lexical-let ((plst (append msg `(:marker ,jump-pos))))
                            (lambda () (weechat/sauron-action plst)))))))

  (defun weechat/handle-message (buffer-ptr)
    (let ((raw-line  (buffer/last-line (weechat--emacs-buffer buffer-ptr)))
          (buffer-hash (weechat-buffer-hash buffer-ptr)))
      (when (s-matches? weechat/match-line-regex raw-line))
      (let ((msg (weechat/parse-line raw-line))
            (buffer-facts
             (list
              :short_name (gethash "short_name" buffer-hash)
              :emacs/buffer (gethash :emacs/buffer buffer-hash)))
            (buffer-local-facts
             (apply #'append
                    (mapcar
                     (lambda (x)
                       (list (make-symbol (concat ":" (car x))) (cdr x)))
                     (gethash "local_variables" buffer-hash))))
            (prio 2))

        (when (weechat/fresh-channel-event
               (plist-get buffer-facts :short-name))
          (incf prio))

        (weechat/sauron-add-event
         (append msg buffer-facts buffer-local-facts) prio))))

  (add-hook 'weechat-message-post-receive-functions
            'weechat/handle-message)

  (setq weechat-modules '(weechat-button
                          weechat-tracking
                          weechat-color
                          weechat-complete
                          weechat-notifications
                          weechat-read-marker
                          weechat-speedbar)))


;;;_ , which-key

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
  :init
  (progn
    (hook-into-modes #'whitespace-mode
                     'prog-mode-hook
                     'c-mode-common-hook)

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
  :bind (("C-x w" . fixup-whitespace)
         ("C-c w" . whitespace-cleanup))
  :config
  (progn
    (setq whitespace-style '(face trailing space-before-tab empty))
    (remove-hook 'find-file-hooks 'whitespace-buffer)
    (remove-hook 'kill-buffer-hook 'whitespace-buffer)))

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

(use-package ivy-purpose
  :after window-purpose
  :config
  (progn
    (defun ivy-switch-buffer-and-select-window ()
      "Use `ivy-switch-buffer' and then select the buffer's window."
      (interactive)
      (select-window (get-buffer-window (ivy-switch-buffer))))
    (define-purpose-prefix-overload purpose-switch-buffer-overload
      '(ivy-switch-buffer-and-select-window
        ivy-purpose-switch-buffer-without-purpose
        ivy-purpose-switch-buffer-with-purpose))))

;;;_ , winner

(use-package winner
  :config
  (progn
    (winner-mode 1)

    (bind-key "M-N" 'winner-redo)
    (bind-key "M-P" 'winner-undo)))

;;;_ , workgroups

;; (use-package workgroups2
;;   ;; BULK-ENSURE :ensure t
;;   :unless noninteractive
;;   :init
;;   (progn
;;     (setq wg-prefix-key (kbd "C-c e"))
;;     (setq wg-session-file "~/.emacs.d/.emacs_workgroups"))
;;   :config
;;   (add-hook 'after-init-hook #'(lambda () (workgroups-mode 1)))
;;   )

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

;;;_ , yasnippet

(use-package yasnippet
  :unless noninteractive
  :diminish yas-minor-mode
  :bind (("C-c y d" . yas-load-directory)
         ("C-c y i" . yas-insert-snippet)
         ("C-c y f" . yas-visit-snippet-file)
         ("C-c y n" . yas-new-snippet)
         ("C-c y t" . yas-tryout-snippet)
         ("C-c y l" . yas-describe-tables)
         ("C-c y g" . yas/global-mode)
         ("C-c y m" . yas/minor-mode)
         ("C-c y a" . yas-reload-all)
         ("C-c y x" . yas-expand))
  :bind (:map yas-keymap
              ("C-i" . yas-next-field-or-maybe-expand))
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :config
  (defun yas-new-snippet (&optional choose-instead-of-guess)
    (interactive "P")
    (let ((guessed-directories (yas-guess-snippet-directories)))
      (switch-to-buffer "*new snippet*")
      (erase-buffer)
      (kill-all-local-variables)
      (snippet-mode)
      (set (make-local-variable 'yas-guessed-modes)
           (mapcar #'(lambda (d) (intern (yas-table-name (car d))))
                   guessed-directories))
      (unless (and choose-instead-of-guess
                   (not (y-or-n-p "Insert a snippet with useful headers? ")))
        (yas-expand-snippet
         (concat "\n"
                 "# -*- mode: snippet -*-\n"
                 "# name: $1\n"
                 "# --\n"
                 "$0\n")))))

  (yas-load-directory
   (ensure-user-dir "snippets/"))
  (yas-global-mode +1))

(use-package yasnippet-snippets
  :after yasnippet
  )

;;;_ , yasnippet-backsolve

(use-package yasnippet-backsolve
  :disabled t
  :unless noninteractive
  :load-path "/home/emacs/yasnippet-backsolve"
  :after yasnippet
  :commands ysbkslv-backsolve)

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
  (setq yankpad-file "/var/shared-elpa/org/yankpad.org")
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

(message "Init done")

(when window-system
  (let ((elapsed (float-time (time-subtract (current-time)
                                            emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed))

  (add-hook 'after-init-hook
            `(lambda ()
               (let ((elapsed (float-time (time-subtract (current-time)
                                                         emacs-start-time))))
                 (message "Loading %s...done (%.3fs) [after-init]"
                          ,load-file-name elapsed)))
            t))

;;   mode: allout
;;   outline-regexp: "^;;;_\\([,. ]+\\)"
;; End:

;;; init.el ends here
