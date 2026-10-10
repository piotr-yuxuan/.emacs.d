;;; 091.el --- Expected skips owned by plan 091  -*- mode: lisp-data -*-

;;; Commentary:
;; The PTY scenarios of the terminal parity matrix
;; (tests/scenarios/l3-pty-*.el).  See 085.el for the format.
;;
;; A gterm scenario skips when gterm's module is not built
;; (`:skip-unless-gterm-module').  No reason of the harness names a
;; missing file, so the entry names zig, without which the module
;; cannot be built: with zig and without the module, the skip fails the
;; run and its message says to build the module.

;;; Entries:

("\\`PTY gterm: " (:missing-executable "zig"))
("\\`PTY [a-z]+: .*\\_<btop\\_>" (:missing-executable "btop"))
