;;; local.el --- Fixture machine-local overrides for the T1 session -*- lexical-binding: t -*-

;; tests/integrity/session/driver.py copies this directory into the
;; init directory of the T1 session, where the configuration loads
;; local.el as it loads the user's own.  It overrides the GTD file as
;; local.el.example documents.

(setq my/gtd-file (expand-file-name "gtd.org" (file-name-directory load-file-name)))

;;; local.el ends here
