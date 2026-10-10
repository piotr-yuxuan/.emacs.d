;;; t1-checks-test.el --- Logic tests of the session checks (T1) -*- lexical-binding: t -*-

;;; Commentary:
;; Each check of `t1-checks.el' runs on two fixtures under
;; tests/integrity/fixtures/session/: CHECK-defect.el holds exactly the
;; defects the check must report, CHECK-clean.el the near misses it
;; must stay silent on.  Both are only read; live.el, loaded here,
;; defines the keymaps, commands, modes and variables they name.  The
;; expected subjects come from the defects the fixtures seed, not from
;; the implementation.

;;; Code:

(require 'buttercup)

(let ((here (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path here)
  (add-to-list 'load-path (expand-file-name ".." here))
  (add-to-list 'load-path (expand-file-name "../fixtures/session/lib" here)))

(require 't1-checks)

(defvar epg-pinentry-mode)

(defconst my/t1-test-fixtures
  (expand-file-name "../fixtures/session/"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "The fixtures of the session checks.")

(load (expand-file-name "live.el" my/t1-test-fixtures) nil t)

(defun my/t1-test-forms (name)
  "Return the forms of the fixture NAME.el."
  (my/integrity-forms (expand-file-name (concat name ".el") my/t1-test-fixtures)))

(defun my/t1-test-maps (name)
  "Return the keymaps the fixture NAME.el binds keys in."
  (seq-uniq (mapcar (lambda (row) (plist-get row :map))
                    (my/integrity-bindings (my/t1-test-forms name)))))

(describe "The ledger judge"
  (it "passes when the ledger lists exactly the subjects found"
    (expect (my/t1-judge 'check '("a") '((check "a" "CF-01" "reason")
                                          (other "b" "CF-02" "reason")))
            :not :to-throw))

  (it "fails on a subject the ledger does not list"
    (expect (my/t1-judge 'check '("a" "b") '((check "a" "CF-01" "reason")))
            :to-throw 'buttercup-failed))

  (it "fails on a listed subject no longer found"
    (expect (my/t1-judge 'check nil '((check "a" "CF-01" "reason")))
            :to-throw 'buttercup-failed)))

(describe "The buffer-line reader"
  (it "reads warnings without their icon, with plain quotes"
    (with-current-buffer (get-buffer-create " *t1fx-warnings*")
      (insert "⛔ Warning (t1fx): ‘x’ is wrong\n\nError (t1fx): y\n"))
    (unwind-protect
        (expect (my/t1-buffer-lines " *t1fx-warnings*")
                :to-equal '("Warning (t1fx): 'x' is wrong" "Error (t1fx): y"))
      (kill-buffer " *t1fx-warnings*")))

  (it "reads nothing from a buffer that does not exist"
    (expect (my/t1-buffer-lines " *t1fx-absent*") :to-equal nil)))

(describe "The loaded-feature check"
  (it "reports the features that are loaded, and only those"
    (expect (my/t1-loaded-features '(t1fx-library t1fx-never-provided) features)
            :to-equal '("t1fx-library"))
    (expect (my/t1-loaded-features '(t1fx-never-provided) features) :to-equal nil))

  (it "judges against the list of features it is given, in the order wanted"
    (expect (my/t1-loaded-features '(t1fx-a t1fx-b t1fx-c) '(t1fx-c t1fx-a))
            :to-equal '("t1fx-a" "t1fx-c"))
    (expect (my/t1-loaded-features '(t1fx-library) nil) :to-equal nil)))

(describe "The fill-column check"
  ;; In a buffer where fill-column is not local, let binds its default.
  (it "reports a default other than 120"
    (with-temp-buffer
      (let ((fill-column 70))
        (expect (my/t1-fill-column-subjects) :to-equal '("default 70")))))

  (it "stays silent on the default 120"
    (with-temp-buffer
      (let ((fill-column 120))
        (expect (my/t1-fill-column-subjects) :to-equal nil)))))

(describe "The GPG prompt check"
  (it "reports an Emacs-side pinentry mode"
    (let ((epg-pinentry-mode 'loopback))
      (expect (my/t1-gpg-prompt-subjects) :to-equal '("epg-pinentry-mode loopback"))))

  (it "stays silent when the mode is unset and pinentry is not loaded"
    (let ((epg-pinentry-mode nil))
      (expect (my/t1-gpg-prompt-subjects) :to-equal nil))))

(describe "The error-line reader"
  (it "keeps the lines that report an error"
    (expect (my/t1-error-lines
             '("Error running timer 'foo': (void-function bar)"
               "error in process sentinel: t1fx: exited abnormally"
               "Cleaning up the recentf list...done (0 removed)"
               "flycheck-error-list-mode is off"
               "No errors, 3 warnings"
               "Errors are shown in the margin"))
            :to-equal '("Error running timer 'foo': (void-function bar)"
                        "error in process sentinel: t1fx: exited abnormally")))

  (it "stays silent on a clean startup"
    (expect (my/t1-error-lines '("Starting Emacs daemon."
                                 "When done with this frame, type C-x 5 0"))
            :to-equal nil)))

(describe "The force-load"
  (it "reports a feature, or a bound command's library, that cannot be loaded"
    (expect (mapcar #'car (my/t1-force-load (my/t1-test-forms "cannot-load-defect")))
            :to-equal '(t1fx-absent-feature t1fx-absent-library))
    (expect (featurep 'gterm) :to-be nil))

  (it "stays silent when every feature loads, and loads bound commands and hooked modes"
    (expect (my/t1-force-load (my/t1-test-forms "cannot-load-clean")) :to-equal nil)
    (expect (autoloadp (symbol-function 't1fx-menu-command)) :to-be nil)
    (expect (autoloadp (symbol-function 't1fx-hooked-mode)) :to-be nil)))

(describe "The unjudged-binding check"
  (it "reports a binding into an unbound map, or to a command not loaded"
    (expect (my/t1-unjudged-bindings
             (my/integrity-bindings (my/t1-test-forms "unjudged-binding-defect")) nil)
            :to-equal '("keymap t1fx-unloaded-map" "command t1fx-absent-command")))

  (it "stays silent on loaded bindings and on those of skipped packages"
    (expect (my/t1-unjudged-bindings
             (my/integrity-bindings (my/t1-test-forms "unjudged-binding-clean"))
             '(t1fx-skipped t1fx-absent-library))
            :to-equal nil)))

(describe "The dangling-binding check"
  (it "reports bindings that are not commands, under prefixes too"
    (expect (my/t1-dangling-bindings (my/t1-test-maps "dangling-binding-defect"))
            :to-have-same-items-as
            '("t1fx-dangling-map C-c b t1fx-missing"
              "t1fx-dangling-map C-c d t1fx-ghost"
              "t1fx-dangling-map C-c l lambda"
              "t1fx-dangling-map C-c p q t1fx-plain")))

  (it "stays silent on commands and on keymaps not loaded"
    (expect (my/t1-dangling-bindings (my/t1-test-maps "dangling-binding-clean"))
            :to-equal nil)))

(describe "The undocumented-command check"
  (it "reports a bound command without a docstring"
    (expect (my/t1-undocumented-commands
             (my/integrity-bindings (my/t1-test-forms "undocumented-command-defect")))
            :to-equal '("t1fx-undocumented")))

  (it "stays silent on documented and undefined commands"
    (expect (my/t1-undocumented-commands
             (my/integrity-bindings (my/t1-test-forms "undocumented-command-clean")))
            :to-equal nil)))

(describe "The mu4e-binding check"
  (it "reports keys bound to mu4e commands, in any map"
    (expect (my/t1-mu4e-bindings
             (my/integrity-bindings (my/t1-test-forms "mu4e-binding-defect")))
            :to-equal '("global C-c m mu4e"
                        "t1fx-clean-map C-c u mu4e-update-mail-and-index")))

  (it "stays silent on other commands in mu4e maps and on removed keys"
    (expect (my/t1-mu4e-bindings
             (my/integrity-bindings (my/t1-test-forms "mu4e-binding-clean")))
            :to-equal nil)))

(describe "The variable check"
  (it "reports undeclared and obsolete variables, and what it did not check"
    (let ((forms (my/t1-test-forms "variables-defect")))
      (expect (my/t1-variable-defects (my/integrity-set-variables forms) '(t1fx-skipped)
                                      (cons 't1fx-lazy-option (my/t1-unrun-variables forms)))
              :to-equal '(:undeclared ("t1fx-ghost-variable" "t1fx-local-ghost"
                                       "t1fx-twice-ghost" "t1fx-flag-ghost")
                          :obsolete ("t1fx-old-variable")
                          :not-checked ("t1fx-lazy-option" "t1fx-skipped-option"
                                        "t1fx-elsewhere-variable" "t1fx-never-variable"
                                        "t1fx-flagged-variable")))))

  (it "stays silent on declared, current variables"
    (let ((forms (my/t1-test-forms "variables-clean")))
      (expect (my/t1-variable-defects (my/integrity-set-variables forms) nil
                                      (my/t1-unrun-variables forms))
              :to-equal '(:undeclared nil :obsolete nil :not-checked nil)))))

(describe "The variable library loader"
  (it "loads the library a variable names, but not a denied one"
    (let ((my/t1-deny-list '((t1fx-denied . "the fixture of a denied library"))))
      (expect (my/t1-load-variable-libraries '(t1fx-lazy-option t1fx-denied-option
                                               t1fx-ghost-variable t1fx-declared))
              :to-equal '(t1fx-denied-option)))
    (expect (custom-variable-p 't1fx-lazy-option) :to-be-truthy)
    (expect (boundp 't1fx-denied-loaded) :to-be nil)))

(describe "The global-mode check"
  (it "reports a global mode turned on at startup that is off"
    (expect (my/t1-global-modes-off (my/t1-test-forms "global-mode-off-defect"))
            :to-equal '("t1fx-off-mode")))

  (it "stays silent on modes on, local, turned off or hooked elsewhere"
    (expect (my/t1-global-modes-off (my/t1-test-forms "global-mode-off-clean"))
            :to-equal nil)))

(defconst my/t1-test-remaps '((t1fx-old-mode . t1fx-parent-mode)
                               (t1fx-parent-mode . t1fx-child-mode)
                               (t1fx-legacy-mode . t1fx-ts-mode)
                               (t1fx-legacy-child-mode . t1fx-ts-mode)
                               (t1fx-styles-mode . t1fx-styles-ts-mode))
  "The remaps of the dead-hook fixtures.")

(describe "The dead-hook check"
  (it "reports hooks of modes that do not exist, are aliases or are remapped away"
    (let ((major-mode-remap-alist my/t1-test-remaps))
      (expect (my/t1-dead-hooks (my/t1-test-forms "dead-hook-defect"))
              :to-equal '("t1fx-absent-mode-hook" "t1fx-gone-mode-hook"
                          "t1fx-old-mode-hook" "t1fx-alias-mode-hook"
                          "t1fx-legacy-mode-hook"))))

  (it "stays silent on hooks that run, in a derived mode too, other hooks, and denied packages"
    (let ((major-mode-remap-alist my/t1-test-remaps))
      (expect (my/t1-dead-hooks (my/t1-test-forms "dead-hook-clean")) :to-equal nil))))

(describe "The foreign-command check"
  (it "reports :commands entries the package does not define"
    (expect (my/t1-foreign-commands (my/t1-test-forms "foreign-command-defect") nil)
            :to-equal '("t1fx-library t1fx-library-absent" "t1fx-library ignore")))

  (it "stays silent on the package's own commands and on skipped packages"
    (expect (my/t1-foreign-commands (my/t1-test-forms "foreign-command-clean") '(gterm))
            :to-equal nil)))

(describe "The repeated-hook check"
  (it "reports a function on the hooks of a mode and of its parent"
    (expect (my/t1-repeated-hook-functions 't1fx-child-mode)
            :to-equal '("t1fx-child-mode t1fx-hook-function")))

  (it "stays silent when each hook of the chain adds its own functions"
    (expect (my/t1-repeated-hook-functions 't1fx-other-mode) :to-equal nil)))

(describe "The mode check of visited files"
  (it "reports the files whose mode hooks turn the mode on"
    (expect (my/t1-buffers-with-mode
             't1fx-local-mode
             (list (expand-file-name "notes.t1fx-child" temporary-file-directory)
                   (expand-file-name "notes.t1fx-other" temporary-file-directory)))
            :to-equal '("t1fx-child-mode")))

  (it "stays silent when no hook turns it on"
    (expect (my/t1-buffers-with-mode
             't1fx-local-mode
             (list (expand-file-name "notes.t1fx-other" temporary-file-directory)))
            :to-equal nil)))

(describe "The cited-key check"
  :var (subjects)
  (before-all
    (require 'org)
    ;; A checkout inside a docs/work/ directory of its own: only the
    ;; names under the root decide which documents are left out.
    (let ((temporary (make-temp-file "t1fx-" t)))
      (unwind-protect
          (let ((root (expand-file-name "docs/work/checkout/" temporary))
                (global-map (define-keymap "<f19> j" (lambda () (interactive))))
                (override-global-map (make-sparse-keymap))
                (my/t1-key-reading-commands '((t1fx-reader . "it reads a key"))))
            (copy-directory (expand-file-name "cited/" my/t1-test-fixtures) root nil t t)
            (setq subjects (my/t1-unbound-cited-keys
                            root
                            '((:map global :key "<f19> j" :command t1fx-clean-map :keymap t))
                            '(("docs/howto/table.org" t1fx-clean-map)))))
        (delete-directory temporary t))))

  (it "reads verbatim and code objects as Org does, once each, alternatives apart"
    ;; No keymap of this test binds them.
    (expect (seq-filter (lambda (subject) (string-prefix-p "docs/guide.org " subject)) subjects)
            :to-equal (mapcar (lambda (key) (concat "docs/guide.org " key))
                              '("C-c a" "<f8>" "C-x 8 RET" "S-<f2>" "C-M-=" "C-c l" "C-c C-l"
                                "C-c P u" "C-=" "C-x" "C-c x" "M-x" "<f5>" "C-c b" "C-c q z"))))

  (it "counts a key bound when a keymap the document is about runs something on it"
    (expect (seq-filter (lambda (subject) (string-prefix-p "docs/modes.org " subject)) subjects)
            :to-equal (mapcar (lambda (key) (concat "docs/modes.org " key))
                              '("C-c a g" "C-c b" "C-c d" "C-c p q" "C-c q z" "<f19> o"
                                "<f19> l" "C-c y" "<f19> j C-c z"))))

  (it "reads the keymaps the table names, and no plan nor keys.org"
    (expect (seq-remove (lambda (subject)
                          (string-match-p "\\`docs/\\(?:guide\\|modes\\)\\.org " subject))
                        subjects)
            :to-equal '("docs/howto/table.org <f19> d"))))

(describe "The minor-mode key check"
  (it "reports the keys the map of a global minor mode that is on binds"
    (expect (my/t1-minor-mode-key-shadows (my/t1-global-minor-mode-maps)
                                          '("<f19> m" "<f19> x"))
            :to-equal '("t1fx-on-mode <f19> m")))

  (it "stays silent on the maps of modes that are off or buffer-local"
    (expect (my/t1-minor-mode-key-shadows (my/t1-global-minor-mode-maps)
                                          '("<f19> o" "<f19> l"))
            :to-equal nil)))

(defconst my/t1-test-package-maps
  '(t1fx-package-mode-map t1fx-child-package-mode-map t1fx-greedy-mode-map)
  "The package mode maps of live.el.")

(describe "The package key check"
  :var ((global (define-keymap "C-c y" #'t1fx-command
                               "C-c P" (define-keymap "z" #'t1fx-command))))

  (it "reports the package maps that hide a reserved global key"
    (let ((global-map global))
      (expect (my/t1-package-key-shadows
               (my/integrity-bindings (my/t1-test-forms "package-key-shadow-defect"))
               my/t1-test-package-maps nil)
              :to-equal '("t1fx-greedy-mode-map C-c" "t1fx-package-mode-map C-c q"))))

  (it "stays silent on the same binding, merged prefixes, own keys and allowed maps"
    (let ((global-map global))
      (expect (my/t1-package-key-shadows
               (my/integrity-bindings (my/t1-test-forms "package-key-shadow-clean"))
               my/t1-test-package-maps
               '((t1fx-greedy-mode-map . "sends every key to a terminal")))
              :to-equal nil))))

(describe "The cheatsheet"
  (it "lists each map's keys in order, with what they run and its first doc line"
    (expect (my/t1-cheatsheet (my/integrity-bindings (my/t1-test-forms "cheatsheet"))
                              '(("init.el" ("keymap-set in t1fx: key not resolved" . 12))))
            :to-equal (concat "#+TITLE: Key bindings\n\n"
                              "Generated by =tests/integrity/session/run cheatsheet= from the live\n"
                              "keymaps of a session in which every configured package is loaded.\n"
                              "Do not edit; regenerate it.\n"
                              "\n* global\n\n| Key | Command | Description |\n|-\n"
                              "| =C-c K= | t1fx-clean-map | prefix map |\n"
                              "\n* t1fx-clean-map\n\n| Key | Command | Description |\n|-\n"
                              "| =C-c a= | t1fx-command | Do nothing, as a documented command. |\n"
                              "| =C-c k x= | t1fx-command | Do nothing, as a documented command. |\n"
                              "| =C-c s= | t1fx-keyed | Call it with M-x t1fx-keyed. |\n"
                              "\n* t1fx-dangling-map\n\n| Key | Command | Description |\n|-\n"
                              "| =C-c b= | t1fx-missing | undefined command |\n"
                              "| =C-c d= | t1fx-ghost | undefined command |\n"
                              "| =C-c l= | anonymous command |  |\n"
                              "\n* t1fx-not-loaded-map\n\n| Key | Command | Description |\n|-\n"
                              "| =C-c n= | t1fx-command | package not loaded |\n"
                              "\n* Forms the readers could not resolve\n\n"
                              "- init.el:12: keymap-set in t1fx: key not resolved\n"))))

;;; t1-checks-test.el ends here
