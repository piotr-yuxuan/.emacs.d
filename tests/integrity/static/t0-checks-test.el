;;; t0-checks-test.el --- Logic tests of the static integrity checks -*- lexical-binding: t -*-

;;; Commentary:
;; Each check of `t0-checks.el' runs on two fixtures under
;; tests/integrity/fixtures/static/: CHECK-defect.el holds exactly the
;; defect the check must report, CHECK-clean.el the near misses it must
;; stay silent on.  The expected subjects come from the defects the
;; fixtures seed, not from the implementation.

;;; Code:

(require 'buttercup)

(let ((here (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path here)
  (add-to-list 'load-path (expand-file-name ".." here)))

(require 't0-checks)

(defconst my/integrity-test-fixtures
  (expand-file-name "../fixtures/static/"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "The fixtures of the static checks.")

(defun my/integrity-test-forms (name)
  "Return the forms of the fixture NAME.el."
  (my/integrity-forms (expand-file-name (concat name ".el") my/integrity-test-fixtures)))

(defun my/integrity-test-bindings (name)
  "Return the bindings of the fixture NAME.el."
  (my/integrity-bindings (my/integrity-test-forms name)))

(describe "The tangle check"
  :var ((org (expand-file-name "straight/build/org" my/integrity-live-directory)))

  (it "reports a generated file edited by hand"
    (expect (my/integrity-tangle-differences
             (expand-file-name "tangle/defect" my/integrity-test-fixtures)
             '("early-init.el" "init.el") org)
            :to-equal '("init.el")))

  (it "stays silent when the files are what README.org tangles"
    (expect (my/integrity-tangle-differences
             (expand-file-name "tangle/clean" my/integrity-test-fixtures)
             '("early-init.el" "init.el") org)
            :to-equal nil))

  (it "reports a file the tangle does not produce"
    (expect (my/integrity-tangle-differences
             (expand-file-name "tangle/clean" my/integrity-test-fixtures)
             '("init.el" "late-init.el") org)
            :to-equal '("late-init.el"))))

(describe "The autoloads reader"
  (it "keeps the packages that load only on use, with what loads them"
    (expect (my/integrity-package-autoloads
             '(fixture-inert fixture-hooking fixture-missing dired lsp-tailwindcss)
             (expand-file-name "packages" my/integrity-test-fixtures))
            :to-equal '((fixture-inert fixture-inert-command fixture-inert-mode)
                        (fixture-missing)))))

(describe "The never-loads check"
  (it "reports blocks that wait for a load nothing triggers"
    (expect (my/integrity-never-loading
             (my/integrity-test-forms "never-loads-defect")
             '((fixture-after fixture-after-mode)
               (fixture-config global-fixture-config-mode)))
            :to-equal '("fixture-after" "fixture-config")))

  (it "stays silent when use-package-always-defer is not set"
    (expect (my/integrity-never-loading
             (cdr (my/integrity-test-forms "never-loads-defect"))
             '((fixture-after fixture-after-mode)
               (fixture-config global-fixture-config-mode)))
            :to-equal nil))

  (it "stays silent on blocks something loads"
    (expect (my/integrity-never-loading
             (my/integrity-test-forms "never-loads-clean")
             '((fixture-demand fixture-demand-mode) (fixture-idle fixture-idle-mode)
               (fixture-eager) (fixture-hook fixture-hook-mode)
               (fixture-bind fixture-bind-command) (fixture-commands fixture-commands-list)
               (fixture-init fixture-init-mode) (fixture-custom fixture-custom-mode)
               (fixture-elsewhere fixture-elsewhere-mode) (fixture-required)
               (fixture-plain) (fixture-disabled fixture-disabled-mode)))
            :to-equal nil)))

(describe "The duplicate-package check"
  (it "reports a package declared twice"
    (expect (my/integrity-duplicate-packages (my/integrity-test-forms "duplicate-package-defect"))
            :to-equal '("fixture-twice")))

  (it "stays silent on packages declared once and mentioned twice"
    (expect (my/integrity-duplicate-packages (my/integrity-test-forms "duplicate-package-clean"))
            :to-equal nil)))

(describe "The redundant-default check"
  (it "reports :ensure t and :defer t that repeat the defaults"
    (expect (my/integrity-redundant-defaults (my/integrity-test-forms "redundant-default-defect"))
            :to-equal '("fixture-noisy :ensure t" "fixture-noisy :defer t")))

  (it "stays silent when the keywords change the defaults"
    (expect (my/integrity-redundant-defaults (my/integrity-test-forms "redundant-default-clean"))
            :to-equal nil)))

(describe "The duplicate-binding check"
  (it "reports a key bound twice in one map"
    (expect (my/integrity-duplicate-bindings (my/integrity-test-bindings "duplicate-binding-defect"))
            :to-equal '("global C-c f")))

  (it "stays silent on one key in two maps and on a removed key bound again"
    (expect (my/integrity-duplicate-bindings (my/integrity-test-bindings "duplicate-binding-clean"))
            :to-equal nil)))

(describe "The untypable-key check"
  (it "reports Shift with a lowercase letter"
    (expect (my/integrity-untypable-keys (my/integrity-test-bindings "untypable-key-defect"))
            :to-equal '("global S-f" "fixture-mode-map M-S-g")))

  (it "stays silent on Control-Shift, shifted function keys and uppercase letters"
    (expect (my/integrity-untypable-keys (my/integrity-test-bindings "untypable-key-clean"))
            :to-equal nil)))

(describe "The lambda-binding check"
  (it "reports lambdas bound with define-key and :bind"
    (expect (my/integrity-lambda-bindings (my/integrity-test-bindings "lambda-binding-defect"))
            :to-equal '("fixture-mode-map M-<left>" "fixture-mode-map M-<right>")))

  (it "stays silent on named commands and on lambdas that are not bound"
    (expect (my/integrity-lambda-bindings (my/integrity-test-bindings "lambda-binding-clean"))
            :to-equal nil)))

(describe "The protected-key check"
  (it "reports protected keys rebound globally, or hidden under a prefix"
    (expect (my/integrity-protected-key-shadows (my/integrity-test-bindings "protected-key-defect"))
            :to-equal '("global C--" "global M-0" "override-global-map C-x f"
                        "global C-x t")))

  (it "stays silent on the allowed overrides, mode maps and nearby keys"
    (expect (my/integrity-protected-key-shadows (my/integrity-test-bindings "protected-key-clean"))
            :to-equal nil)))

(describe "The mode-map-shadow check"
  (it "reports mode-map keys over a global key of the configuration"
    (expect (my/integrity-mode-map-shadows (my/integrity-test-bindings "mode-map-shadow-defect"))
            :to-equal '("python-base-mode-map C-c t" "fixture-mode-map C-c a b")))

  (it "stays silent on the per-language prefixes, prefix keymaps and other maps"
    (expect (my/integrity-mode-map-shadows (my/integrity-test-bindings "mode-map-shadow-clean"))
            :to-equal nil)))

;;; t0-checks-test.el ends here
