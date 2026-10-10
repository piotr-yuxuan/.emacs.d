;;; 085.el --- Expected skips owned by plan 085  -*- mode: lisp-data -*-

;;; Commentary:
;; One file per plan.  tests/run reads these files; it never evaluates
;; them.  Each top-level form is one entry:
;;
;;   (SPEC-REGEXP REASON)
;;
;; SPEC-REGEXP is an Emacs regexp matched, case-sensitively, against the
;; full name of a spec ("describe" names and the "it" name, joined by
;; spaces).  REASON is checked inside the session that ran the spec:
;;
;;   (:missing-executable "NAME")  NAME is not on that session's exec-path
;;   (:no-gui)                     the session has no graphical frame
;;   (:platform SYSTEM-TYPE)       `system-type' is SYSTEM-TYPE, e.g. darwin
;;
;; A skipped spec passes only when an entry matches it and its reason
;; is true.  An entry is stale, and fails the run, when its reason is
;; true and the specs it matches ran without skipping; in a full,
;; unfiltered, graphical run, an entry that matches no spec is stale
;; too.  Specs disabled with xit or xdescribe always fail.
;;
;; Scenario files that declare ";; scenario-display: graphic" need no
;; entry: under EMACS_TEST_GUI=0 the runner skips them as a whole.

;;; Entries:
