;;; 120.el --- Expected skips owned by plan 120  -*- mode: lisp-data -*-

;;; Commentary:
;; The scenarios of tests/scenarios/l3-toml-compose.el wait for real
;; language servers; a row skips when its server is missing.  See
;; 085.el for the format.

;;; Entries:

("\\`TOML: " (:missing-executable "tombi"))
("\\`COMPOSE: " (:missing-executable "docker-language-server"))
