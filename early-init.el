;; early-init.el -- post-27 init file
;; -*- lexical-binding: t -*-
;;; Commentary:
;;; Set up package system parameters.

;;; Code:

(defconst emacs-start-time (current-time))

(defvar file-name-handler-alist-old file-name-handler-alist)

(add-hook 'after-init-hook
          `(lambda ()
             (setq file-name-handler-alist file-name-handler-alist-old
                   gc-cons-threshold 800000
                   gc-cons-percentage 0.1)
             (message "calling garbage-collect")
             (garbage-collect) t))

(setq straight-base-dir                "/var/shared-elpa"
      package-enable-at-startup        nil
      file-name-handler-alist          nil
      file-name-handler-alist          nil
      message-log-max                  16384
      gc-cons-threshold                most-positive-fixnum
      gc-cons-percentage               0.6
      auto-window-vscroll              nil
      load-prefer-newer                t
      large-file-warning-threshold     10000000
      straight-check-for-modifications 'live-with-find
      straight-cache-autoloads         t
      straight-use-package-by-default  t
      install-run                      nil)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
                         straight-base-dir))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/\
raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(setq use-package-verbose t)

(use-package bind-key)
;; (use-package no-littering)
(use-package use-package-ensure-system-package)

(provide 'early-init)
;;; early-init ends here
