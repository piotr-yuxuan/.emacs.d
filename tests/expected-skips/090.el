;;; 090.el --- Expected skips owned by plan 090  -*- mode: lisp-data -*-

;;; Commentary:
;; The diagnostic scenarios of tests/scenarios/l3-diagnostics*.el run
;; real checkers.  A row skips when `:executables-found', or the
;; `:wait-for' of its language server, finds a program missing; see
;; tests/expected-skips/085.el for the format.

;;; Entries:

("\\`DIAG-\\(?:SHELL\\|NEXT\\)" (:missing-executable "bash-language-server"))
("\\`DIAG-\\(?:SHELL\\|NEXT\\)" (:missing-executable "shellcheck"))
("\\`DIAG-PYTHON: " (:missing-executable "basedpyright-langserver"))
("\\`DIAG-TERRAFORM" (:missing-executable "tofu-ls"))
("\\`DIAG-TERRAFORM-PROJECT: " (:missing-executable "tofu"))
("\\`DIAG-\\(?:MARKDOWN\\|ORG-CHAIN\\)" (:missing-executable "ltex-ls-plus"))
("\\`DIAG-MARKDOWN" (:missing-executable "marksman"))
("\\`DIAG-\\(?:FRENCH\\|ENGLISH\\): " (:missing-executable "grammalecte-cli.py"))
("\\`DIAG-\\(?:FRENCH\\|ENGLISH\\): " (:missing-executable "python3"))
("\\`DIAG-ORG-WITHOUT-LTEX: " (:missing-executable "grammalecte-cli.py"))
("\\`DIAG-ORG-WITHOUT-LTEX: " (:missing-executable "python3"))
