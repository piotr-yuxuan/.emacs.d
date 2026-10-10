;;; l1-python-web-opentofu.el --- Plan 076 editing lifecycle -*- lexical-binding: t -*-

;;; Commentary:
;; Focused L1 coverage for the user-visible lifecycle introduced by plan
;; 076: which modes and tools a Python, web or OpenTofu file gets when
;; opened.  Real language-server and formatter processes are exercised
;; in L3 files; the Python helper functions are logic tests in
;; `tests/python-helpers-test.el'.

;;; Code:

(require 'scenario)

;; The configuration asks before it trusts this project-local form.
;; The fixture is part of this repository, so this session, like a
;; user who answered once, trusts only this exact form.
(add-to-list 'safe-local-eval-forms '(datastar-turn-on-mode))

(scenario
 :describe "BL1-PYTHON-PET: Python projects activate PET and Apheleia"
 :given ((:name "uv"
          :project "python-uv"
          :entry "src/app.py")
         (:name "poetry"
          :project "python-poetry"
          :entry "src/app.py"))
 :when ((:open))
 :then ((:mode python-mode)
        (:minor-modes-on pet-mode electric-pair-mode apheleia-mode)
        (:variable-equals lsp-pyright-langserver-command "basedpyright")
        (:variable-equals python-shell-dedicated project)
        (:variable-equals corfu-on-exact-match insert)
        (:variable-equals lsp-auto-guess-root t)
        (:variable-equals lsp-guess-root-without-session t)
        (:variable-equals lsp-inlay-hint-enable t)
        (:variable-equals lsp-pyright-basedpyright-inlay-hints-variable-types t)
        (:variable-equals lsp-pyright-basedpyright-inlay-hints-call-argument-names t)
        (:variable-equals lsp-pyright-basedpyright-inlay-hints-function-return-types t)))

(scenario
 :describe "BL1-WEB-STATIC: ordinary HTML keeps tree-sitter and formatting"
 :given ((:name "html"
          :file "public/index.html"
          :text "<main>Hello</main>\n"))
 :when ((:open))
 :then ((:mode html-mode)
        (:minor-modes-on apheleia-mode)
        (:minor-modes-off datastar-mode)))

(scenario
 :describe "BL1-WEB-TEMPLATE: Jinja routes to web-mode without Apheleia"
 :given ((:name "templates-html"
          :file "templates/index.html"
          :text "<p>{{ message }}</p>\n")
         (:name "jinja2"
          :file "view.jinja2"
          :text "<p>{{ message }}</p>\n")
         (:name "j2"
          :file "view.j2"
          :text "<p>{{ message }}</p>\n")
         (:name "djhtml"
          :file "view.djhtml"
          :text "<p>{{ message }}</p>\n"))
 :when ((:open))
 :then ((:mode web-mode)
        (:minor-modes-off apheleia-mode datastar-mode)
        (:variable-equals web-mode-engine "django")))

(scenario
 :describe "BL1-DATASTAR-LOCAL: a trusted project opts in without leaking globally"
 :given ((:name "datastar"
          :project "datastar-web"
          :entry "view.jinja2"))
 :when ((:open))
 :then ((:mode web-mode)
        (:minor-modes-on datastar-mode)
        (:minor-modes-off apheleia-mode)))

(scenario
 :describe "BL1-OPENTOFU: infrastructure files use tofu and conditional TFLint"
 :given ((:name "tf-plain"
          :project "opentofu-plain"
          :entry "main.tf")
         (:name "tofutest-plain"
          :project "opentofu-plain"
          :entry "example.tofutest.hcl"))
 :when ((:open))
 :then ((:mode terraform-mode)
        (:variable-equals terraform-command "tofu")
        (:variable-contains flycheck-disabled-checkers terraform)
        (:variable-contains flycheck-disabled-checkers terraform-tflint)))

(scenario
 :describe "BL1-OPENTOFU-TFLINT: configured projects leave TFLint enabled"
 :given ((:name "tf-linted"
          :project "opentofu-linted"
          :entry "main.tf"))
 :when ((:open))
 :then ((:mode terraform-mode)
        (:variable-equals terraform-command "tofu")
        (:variable-contains flycheck-disabled-checkers terraform)
        (:variable-excludes flycheck-disabled-checkers terraform-tflint)))

(provide 'l1-python-web-opentofu)

;;; l1-python-web-opentofu.el ends here
