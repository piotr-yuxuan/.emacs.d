;;; t0.el --- The static integrity checks of this configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Tier T0 of plan 086, run by `tests/run static' in `emacs --batch -Q'
;; (see `run.el').  Each spec runs one check of `t0-checks.el' over
;; `early-init.el' and `init.el', read as data, and compares the
;; subjects it reports with the known-defects ledger: a listed defect is
;; tolerated, an unlisted one fails, and a listed one the check no
;; longer reports fails as stale.  The failure shows the subjects.

;;; Code:

(require 'buttercup)
(require 't0-checks)
(require 'ledger)

(defconst my/integrity-t0-root
  (expand-file-name "../../../" (file-name-directory (or load-file-name buffer-file-name)))
  "The checkout whose configuration T0 checks.")

(defconst my/integrity-t0-unresolved-allowed
  '(("define-key in gterm: map gterm-mode-map, key (kbd (format \"C-%c\" c)) not resolved"
     . "a dolist sends Ctrl and a letter to the terminal; the keys are generated")
    ("define-key in gterm: map gterm-mode-map, key (kbd (format \"M-%c\" c)) not resolved"
     . "a dolist sends Meta and a letter to the terminal; the keys are generated"))
  "Forms the readers cannot resolve, each with the reason it needs no check.
An alist of (DESCRIPTION . REASON), DESCRIPTION as
`my/integrity-unresolved' gives it.")

(defun my/integrity-t0--expect-ledger (check subjects entries)
  "Expect the SUBJECTS that CHECK reports to be those ENTRIES list for it."
  (expect (my/integrity-ledger-compare check subjects entries)
          :to-equal '(:unexpected nil :stale nil)))

(describe "The static integrity of the configuration (T0)"
  :var (forms bindings entries)
  (before-all
    (setq forms (append (my/integrity-forms (expand-file-name "early-init.el" my/integrity-t0-root))
                        (my/integrity-forms (expand-file-name "init.el" my/integrity-t0-root)))
          bindings (my/integrity-bindings forms)
          entries (my/integrity-ledger-entries
                   (expand-file-name "tests/integrity/known-defects" my/integrity-t0-root))))

  (it "has early-init.el and init.el as README.org tangles them"
    (my/integrity-t0--expect-ledger
     'tangle-sync
     (my/integrity-tangle-differences
      my/integrity-t0-root '("early-init.el" "init.el")
      (expand-file-name "straight/build/org" my/integrity-live-directory))
     entries))

  (it "resolves every binding and after-load form, or knows why not"
    (let ((found (mapcar #'car (my/integrity-unresolved forms)))
          (allowed (mapcar #'car my/integrity-t0-unresolved-allowed)))
      (expect (list :unexpected (seq-difference found allowed)
                    :stale (seq-difference allowed found))
              :to-equal '(:unexpected nil :stale nil))))

  (it "loads every use-package block that waits for its package"
    (my/integrity-t0--expect-ledger
     'never-loads
     (my/integrity-never-loading
      forms (my/integrity-package-autoloads
             (my/integrity-packages forms)
             (expand-file-name "straight/build" my/integrity-live-directory)))
     entries))

  (it "declares each package once"
    (my/integrity-t0--expect-ledger 'duplicate-package (my/integrity-duplicate-packages forms)
                                    entries))

  (it "does not repeat the use-package defaults"
    (my/integrity-t0--expect-ledger 'redundant-default (my/integrity-redundant-defaults forms)
                                    entries))

  (it "binds each key once per map"
    (my/integrity-t0--expect-ledger 'duplicate-binding (my/integrity-duplicate-bindings bindings)
                                    entries))

  (it "binds only keys a keyboard can type"
    (my/integrity-t0--expect-ledger 'untypable-key (my/integrity-untypable-keys bindings)
                                    entries))

  (it "binds named commands, not lambdas"
    (my/integrity-t0--expect-ledger 'lambda-binding (my/integrity-lambda-bindings bindings)
                                    entries))

  (it "keeps the protected vanilla keys"
    (my/integrity-t0--expect-ledger 'protected-key (my/integrity-protected-key-shadows bindings)
                                    entries))

  (it "does not shadow its global keys in mode maps"
    (my/integrity-t0--expect-ledger 'mode-map-shadow (my/integrity-mode-map-shadows bindings)
                                    entries)))

;;; t0.el ends here
