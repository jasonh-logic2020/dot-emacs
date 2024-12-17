;; ;; early-init.el -- post-27 init file
;; ;; -*- lexical-binding: t -*-
;; ;;; Commentary:
;; ;;; Set up package system parameters.

;; ;;; Code:

;; (defconst emacs-start-time (current-time))

;; (defvar file-name-handler-alist-old file-name-handler-alist)

;; (add-hook 'after-init-hook
;;           `(lambda ()
;;              (setq file-name-handler-alist file-name-handler-alist-old
;;                    gc-cons-threshold 800000
;;                    gc-cons-percentage 0.1)
;;              (message "calling garbage-collect")
;;              (garbage-collect) t))

;; (setq  package-user-dir                 "/var/sharedelpa"
;;        package-enable-at-startup        t
;;        file-name-handler-alist          nil
;;        file-name-handler-alist          nil
;;        message-log-max                  16384
;;        gc-cons-threshold                most-positive-fixnum
;;        gc-cons-percentage               0.6
;;        auto-window-vscroll              nil
;;        load-prefer-newer                t
;;        large-file-warning-threshold     10000000)

;; (setq load-prefer-newer t)
;; (load (expand-file-name "local-preinit.el" user-emacs-directory) 'no-error)
;; (setq package-check-signature nil)
;; (unless (and (boundp 'package--initialized) package--initialized)
;;   ;; don't set gnu/org/melpa if the site-local or local-preinit have
;;   ;; done so (e.g. firewalled corporate environments)
;;   (require 'package)
;;   (setq
;;    package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
;;                       ("non-gnu" . "http://elpa.nongnu.org/nongnu/")
;;                       ("melpa-stable" . "http://stable.melpa.org/packages/")
;;                       ("melpa" . "http://melpa.org/packages/"))
;;    package-archive-priorities '(("gnu" . 5)
;;                                 ("non-gnu" . 3)
;;                                 ("melpa-stable" . 2)
;;                                 ("melpa" . 1))))
;; (package-initialize)
;; (setq package-user-dir "/var/sharedelpa"
;;       custom-file (expand-file-name "settings.el" package-user-dir))
;; ;; load custom but ignore error if doesn't exist
;; (load custom-file 'noerror 'nomessage)
;; ;; (unless (or
;; ;;          (require 'use-package nil 'no-error)
;; ;;          package-archive-contents)
;; ;;   (package-refresh-contents)
;; ;;   (package-install 'use-package))
;; (unless (package-installed-p 'vc-use-package)
;;   (package-vc-install "https://github.com/slotThe/vc-use-package"))
;; (require 'use-package)
;; (require 'vc-use-package)

;; ;; (setq elpaca-base-dir                  "var/sharedekpa"
;; ;;       package-enable-at-startup        nil
;; ;;       file-name-handler-alist          nil
;; ;;       file-name-handler-alist          nil
;; ;;       message-log-max                  16384
;; ;;       gc-cons-threshold                most-positive-fixnum
;; ;;       gc-cons-percentage               0.6
;; ;;       auto-window-vscroll              nil
;; ;;       load-prefer-newer                t
;; ;;       large-file-warning-threshold     10000000)

;; ;; (defvar elpaca-installer-version 0.7)
;; ;; (defvar elpaca-directory (expand-file-name "elpaca/" elpaca-base-dir))
;; ;; (defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
;; ;; (defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
;; ;; (defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
;; ;;                               :ref nil :depth 1
;; ;;                               :files (:defaults "elpaca-test.el" (:exclude "extensions"))
;; ;;                               :build (:not elpaca--activate-package)))
;; ;; (let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
;; ;;        (build (expand-file-name "elpaca/" elpaca-builds-directory))
;; ;;        (order (cdr elpaca-order))
;; ;;        (default-directory repo))
;; ;;   (add-to-list 'load-path (if (file-exists-p build) build repo))
;; ;;   (unless (file-exists-p repo)
;; ;;     (make-directory repo t)
;; ;;     (when (< emacs-major-version 28) (require 'subr-x))
;; ;;     (condition-case-unless-debug err
;; ;;         (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
;; ;;                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
;; ;;                                                  ,@(when-let ((depth (plist-get order :depth)))
;; ;;                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
;; ;;                                                  ,(plist-get order :repo) ,repo))))
;; ;;                  ((zerop (call-process "git" nil buffer t "checkout"
;; ;;                                        (or (plist-get order :ref) "--"))))
;; ;;                  (emacs (concat invocation-directory invocation-name))
;; ;;                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
;; ;;                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
;; ;;                  ((require 'elpaca))
;; ;;                  ((elpaca-generate-autoloads "elpaca" repo)))
;; ;;             (progn (message "%s" (buffer-string)) (kill-buffer buffer))
;; ;;           (error "%s" (with-current-buffer buffer (buffer-string))))
;; ;;       ((error) (warn "%s" err) (delete-directory repo 'recursive))))
;; ;;   (unless (require 'elpaca-autoloads nil t)
;; ;;     (require 'elpaca)
;; ;;     (elpaca-generate-autoloads "elpaca" repo)
;; ;;     (load "./elpaca-autoloads")))
;; ;; (add-hook 'after-init-hook #'elpaca-process-queues)
;; ;; (elpaca `(,@elpaca-order))

;; ;; ;; Install use-package support
;; ;; (elpaca elpaca-use-package
;; ;;         ;; Enable use-package :ensure support for Elpaca.
;; ;;         (elpaca-use-package-mode))

;; ;; (setq custom-file (expand-file-name "customs.el" user-emacs-directory))
;; ;; (add-hook 'elpaca-after-init-hook (lambda () (load custom-file 'noerror)))

;; ;; (setq straight-base-dir                "/var/sharedelpa"
;; ;;       package-enable-at-startup        nil
;; ;;       file-name-handler-alist          nil
;; ;;       file-name-handler-alist          nil
;; ;;       message-log-max                  16384
;; ;;       gc-cons-threshold                most-positive-fixnum
;; ;;       gc-cons-percentage               0.6
;; ;;       auto-window-vscroll              nil
;; ;;       load-prefer-newer                t
;; ;;       large-file-warning-threshold     10000000
;; ;;       straight-check-for-modifications 'live-with-find
;; ;;       straight-cache-autoloads         t
;; ;;       straight-use-package-by-default  t
;; ;;       install-run                      nil)

;; ;; (defvar bootstrap-version)
;; ;; (let ((bootstrap-file
;; ;;        (expand-file-name "straight/repos/straight.el/bootstrap.el"
;; ;;                          straight-base-dir))
;; ;;       (bootstrap-version 5))
;; ;;   (unless (file-exists-p bootstrap-file)
;; ;;     (with-current-buffer
;; ;;         (url-retrieve-synchronously
;; ;;          "https://raw.githubusercontent.com/\
;; ;; raxod502/straight.el/develop/install.el"
;; ;;          'silent 'inhibit-cookies)
;; ;;       (goto-char (point-max))
;; ;;       (eval-print-last-sexp)))
;; ;;   (load bootstrap-file nil 'nomessage))

;; ;; (straight-use-package 'use-package)

;; (setq use-package-verbose t
;;       use-package-always-ensure nil)

;; ;; (message "pre length %s" (length load-path))

;; ;; (setq load-path (cl-remove-if
;; ;;                  #'(lambda (x)
;; ;;                      (cl-search "/lisp/org/" x))
;; ;;                  load-path))

;; ;; (message "post length %s" (length load-path))

;; ;; (add-to-list 'load-path "/var/shared-elpa/build/org" t)

;; (use-package bind-key)
;; ;; (use-package no-littering)
;; (use-package use-package-ensure-system-package)

;; (provide 'early-init)
;; ;;; early-init ends here
;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(package-vc-selected-packages
;;    '((vc-use-package :vc-backend Git :url "https://github.com/slotThe/vc-use-package"))))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )
