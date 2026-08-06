;;; early-init.el --- pre-package initialization -*- lexical-binding: t -*-

;;; Commentary:

;; Single source of truth for where packages live.  This must run before any
;; `package-initialize' call, which is why none of it belongs in init.el.
;;
;; The model is package.el's own two-tier scheme:
;;
;;   `package-directory-list'  host-wide set, admin-installed, world-readable.
;;                             Activated by every user, written by none.
;;   `package-user-dir'        per-user overlay under ~/.emacs.d.  The only
;;                             tree `package-install' writes to.
;;
;; Config stays per-user in each ~/.emacs.d; only the package tree is shared.
;; See site-packages.el for the host manifest and the `emacs-admin' group
;; that governs write access to the shared tree.

;;; Code:

(defvar site-package-directory
  (or (getenv "EMACS_SITE_PACKAGE_DIR")
      "/usr/local/share/emacs/site-lisp/elpa/")
  "Host-wide package tree, admin-installed and read-only to ordinary users.
Overridable via the EMACS_SITE_PACKAGE_DIR environment variable so a host
can relocate the shared set without editing this file.")

;; System-wide packages.  A missing directory is harmless: package.el guards
;; every entry with `file-directory-p' (see `package-load-all-descriptors'),
;; so this is safe to set before the shared tree has been built.
(setq package-directory-list (list site-package-directory))

;; Personal overlay.  Anything absent from the shared tree installs here, which
;; keeps `use-package-always-ensure' self-healing rather than fatal.
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))

;; Keep the GPG keyring per-user.  gpg refuses to use a group-writable home
;; directory, so a shared keyring would either warn on every run or have to be
;; loosened in a way gpg treats as unsafe.
(setq package-gnupghome-dir (expand-file-name "gnupg" package-user-dir))

;; Prevent automatic package activation before init.el so that faces.el is
;; loaded before transient.el's defface calls face-attribute on 'shadow.
;; `package-initialize' in init.el handles activation after (require 'faces).
(setq package-enable-at-startup nil)

(provide 'early-init)

;;; early-init.el ends here
