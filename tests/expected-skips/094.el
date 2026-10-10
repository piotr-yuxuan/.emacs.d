;;; 094.el --- Expected skips owned by plan 094  -*- mode: lisp-data -*-

;;; Commentary:
;; The journeys (tests/scenarios/*-journey-*.el) and the K18 formatter
;; scenario.  See 085.el for the format.  A journey skips only when a
;; program it is about is missing; rg and git are requirements, so a
;; missing rg or git fails the journeys that use them.

;;; Entries:

("\\`K18-APHELEIA-PYTHON: " (:missing-executable "isort"))
("\\`K18-APHELEIA-PYTHON: " (:missing-executable "black"))
("\\`J-11: .* in Python " (:missing-executable "basedpyright-langserver"))
("\\`J-11: .* in Clojure " (:missing-executable "clojure-lsp"))
("\\`J-14: " (:missing-executable "clojure"))
("\\`J-14: " (:missing-executable "java"))
("\\`K18-APHELEIA-PYTHON: " (:missing-executable "basedpyright-langserver"))
("\\`K18-APHELEIA-PYTHON: " (:missing-executable "ruff"))
