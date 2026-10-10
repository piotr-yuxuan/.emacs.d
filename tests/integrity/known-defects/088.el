;;; 088.el --- Known defects that plan 088 fixes -*- lexical-binding: t -*-
;; One quoted list of entries (CHECK SUBJECT ID084 REASON); see
;; tests/integrity/ledger.el.  Delete an entry once the check reports
;; it as stale.
'(
  ;; T0, static checks (tests/integrity/static/).

  ;; T1, session checks (tests/integrity/session/).
  (cannot-load "pyim-basedict" "CF-06" "not cloned yet: the first load of pyim in a real session clones it; D6 forbids cloning here"))
