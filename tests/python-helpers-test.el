;;; python-helpers-test.el --- Logic tests of the Python editing helpers -*- lexical-binding: t -*-

;;; Commentary:
;; Logic tests: the decision functions of the Python section of the
;; configuration, called directly.  Stubs stand only at system
;; boundaries: the executables on PATH and the lsp-mode session file.
;; The user-visible lifecycle is in
;; `tests/scenarios/l1-python-web-opentofu.el'.

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'python)
(require 'flycheck)
(require 'lsp-mode)
(require 'lsp-pyright)
(require 'lsp-ruff)

(describe "Python editor tool selection"
  (it "prefers BasedPyright when both Pyright variants are installed"
    (cl-letf (((symbol-function 'executable-find)
               (lambda (executable)
                 (and (member executable '("basedpyright-langserver"
                                           "pyright-langserver"))
                      (concat "/profile/bin/" executable)))))
      (expect (my/python-language-server-command)
              :to-equal "basedpyright")))

  (it "falls back to upstream Pyright"
    (cl-letf (((symbol-function 'executable-find)
               (lambda (executable)
                 (and (equal executable "pyright-langserver")
                      "/profile/bin/pyright-langserver"))))
      (expect (my/python-language-server-command)
              :to-equal "pyright")))

  (it "replaces PET's project Ruff with the host-compatible executable"
    (with-temp-buffer
      (let ((lsp-pyright-langserver-command "/project/.venv/bin/pyright")
            (lsp-ruff-server-command
             '("/project/.venv/bin/ruff" "server")))
        (cl-letf (((symbol-function 'executable-find)
                   (lambda (executable)
                     (pcase executable
                       ("basedpyright-langserver"
                        "/profile/bin/basedpyright-langserver")
                       ("ruff" "/profile/bin/ruff")))))
          (my/python-after-pet)
          (expect lsp-pyright-langserver-command
                  :to-equal "basedpyright")
          (expect lsp-ruff-server-command
                  :to-equal '("/profile/bin/ruff" "server"))
          (expect flycheck-python-ruff-executable
                  :to-equal "/profile/bin/ruff")
          (expect (car (last flycheck-mode-hook))
                  :to-be #'my/python-use-host-ruff))))))

(describe "Python completion resilience"
  (it "installs fuzzy module completion before LSP attaches"
    (with-temp-buffer
      (let ((major-mode 'python-mode))
        (setq-local lsp-completion-mode nil
                    completion-at-point-functions
                    '(python-completion-at-point t))
        (my/python-completion-setup)
        (expect (car completion-at-point-functions)
                :to-be #'my/python-module-capf))))

  (it "upgrades fuzzy module completion when LSP attaches"
    (with-temp-buffer
      (let ((major-mode 'python-mode))
        (setq-local lsp-completion-mode t
                    completion-at-point-functions
                    '(lsp-completion-at-point my/python-module-capf t))
        (my/python-completion-setup)
        (expect (car completion-at-point-functions)
                :to-be #'my/python-completion-at-point)
        (expect (seq-count
                 (lambda (capf)
                   (memq capf '(lsp-completion-at-point
                                 my/python-module-capf)))
                 completion-at-point-functions)
                :to-equal 0))))

  (it "leaves member completion after a dot to the language server"
    (with-temp-buffer
      (insert "requests.")
      (cl-letf (((symbol-function 'my/python-importable-modules)
                 (lambda ()
                   (error "module discovery should not run after a dot"))))
        (expect (my/python-module-capf) :to-be nil)))))

(provide 'python-helpers-test)

;;; python-helpers-test.el ends here
