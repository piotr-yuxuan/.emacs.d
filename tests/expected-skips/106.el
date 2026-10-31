;;; 106.el --- Expected skips owned by plan 106  -*- mode: lisp-data -*-

;;; Commentary:
;; Emacs on macOS trashes files through the system, into the user's own
;; Trash, which a session cannot keep apart: the scenario that needs the
;; freedesktop trash skips there before its keys (`:freedesktop-trash'
;; of tests/helpers/scenario/wait.el).  See 085.el for the format.

;;; Entries:

("\\`BL1-DIRVISH-TRASH: " (:platform darwin))
