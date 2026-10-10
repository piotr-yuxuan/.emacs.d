;;; readers-test.el --- Logic tests of the configuration and ledger readers -*- lexical-binding: t -*-

;;; Commentary:
;; The readers of `config-forms.el' and `ledger.el', shared by T0 and
;; T1, run on fixtures under tests/integrity/fixtures/static/.  The
;; fixture `readers.el' holds one form of each kind the readers know;
;; the expectations restate what each form does when Emacs evaluates it.

;;; Code:

(require 'buttercup)

(let ((here (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name ".." here)))

(require 'config-forms)
(require 'ledger)

(defconst my/integrity-readers-fixtures
  (expand-file-name "../fixtures/static/"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "The fixtures of the readers.")

(describe "The configuration readers"
  :var (forms)
  (before-all
    (setq forms (my/integrity-forms
                 (expand-file-name "readers.el" my/integrity-readers-fixtures))))

  (it "read each top-level form with the line it starts on"
    (expect (mapcar #'cdr forms)
            :to-equal '(4 6 8 9 10 11 12 13 14 16 22 39 42 44 46 48 51 53 55))
    (expect (car (nth 1 forms)) :to-equal '(setq-default fixture-c 3)))

  (it "read one row per binding, in reading order, with its map and source"
    (expect (mapcar (lambda (binding)
                      (list (plist-get binding :map) (plist-get binding :key)
                            (plist-get binding :command) (plist-get binding :source)))
                    (my/integrity-bindings forms))
            :to-equal
            '((global "C-c a" fixture-agenda nil)
              (global "C-c b" fixture-buffers nil)
              (global "C-c c" fixture-capture nil)
              (fixture-mode-map "<remap> <undo>" fixture-undo nil)
              (fixture-mode-map "M-<left>" :lambda nil)
              (fixture-mode-map "C-c e" fixture-eval nil)
              (override-global-map "C-c f" fixture-find nil)
              (global "C-c g" fixture-go fixture-package)
              (fixture-mode-map "C-c h" fixture-here fixture-package)
              (override-global-map "C-c i" fixture-important fixture-package)
              (global "C-c j" fixture-command-map fixture-package)
              (fixture-later-mode-map "C-c k" fixture-later-command fixture-package))))

  (it "mark the :bind-keymap entries"
    (expect (mapcar (lambda (binding) (plist-get binding :key))
                    (seq-filter (lambda (binding) (plist-get binding :keymap))
                                (my/integrity-bindings forms)))
            :to-equal '("C-c j")))

  (it "list every use-package, nested ones included, duplicates kept"
    (expect (my/integrity-packages forms)
            :to-equal '(fixture-package fixture-nested fixture-package)))

  (it "list the features after-load forms wait for, not the :after keywords"
    (expect (my/integrity-after-load-features forms)
            :to-equal '(fixture-base fixture-extra fixture-later fixture-feature)))

  (it "list the variables set, with their package, leaving out local names"
    (expect (my/integrity-set-variables forms)
            :to-equal '((fixture-a) (fixture-b) (fixture-c) (fixture-d)
                        (fixture-e . fixture-package) (fixture-f . fixture-package)
                        (fixture-g . fixture-package) (fixture-h . fixture-package)
                        (fixture-i . fixture-nested) (fixture-j . fixture-nested)
                        (fixture-k))))

  (it "report the forms they cannot resolve, naming the unresolved part"
    (let ((unresolved (my/integrity-unresolved forms)))
      (expect (mapcar #'cdr unresolved) :to-equal '(39 42 44 48 51 55))
      (expect (car (nth 0 unresolved)) :to-match "local-set-key")
      (expect (car (nth 1 unresolved)) :to-match "\"fixture-file\"")
      (expect (car (nth 2 unresolved)) :to-match "(current-local-map)")
      ;; A map held in a local variable is a guess: the dolist binds it.
      (expect (car (nth 3 unresolved)) :to-match "map map,")
      (expect (car (nth 4 unresolved)) :to-match "defvar-keymap")
      (expect (car (nth 5 unresolved)) :to-match "map map,"))))

(describe "The known-defects ledger"
  (it "reads the entries of the NNN.el files, in file order"
    (expect (my/integrity-ledger-entries
             (expand-file-name "ledger/valid" my/integrity-readers-fixtures))
            :to-equal '((never-loads "fixture-a" "CF-05" "nothing loads it")
                        (protected-key "global C--" "CF-12" "bound globally")
                        (protected-key "global M-0" "DD-02" "bound globally"))))

  (it "rejects an entry keyed by line number, naming its file"
    (expect (condition-case err
                (my/integrity-ledger-entries
                 (expand-file-name "ledger/malformed" my/integrity-readers-fixtures))
              (error (error-message-string err)))
            :to-match "malformed/089\\.el: malformed entry"))

  (it "passes a check whose findings are exactly the listed ones"
    (expect (my/integrity-ledger-compare
             'protected-key '("global M-0" "global C--")
             '((protected-key "global C--" "CF-12" "r") (protected-key "global M-0" "DD-02" "r")
               (never-loads "global C-x" "CF-05" "r")))
            :to-equal '(:unexpected nil :stale nil)))

  (it "fails on an unlisted finding and on a listed one no longer found"
    (expect (my/integrity-ledger-compare
             'protected-key '("global C-x t 1" "global C--")
             '((protected-key "global C--" "CF-12" "r") (protected-key "global M-0" "DD-02" "r")
               (never-loads "global C-x t 1" "CF-05" "r")))
            :to-equal '(:unexpected ("global C-x t 1") :stale ("global M-0")))))

;;; readers-test.el ends here
