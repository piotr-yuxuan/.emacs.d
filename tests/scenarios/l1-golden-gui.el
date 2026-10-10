;;; l1-golden-gui.el --- Golden approval of faces and indentation -*- lexical-binding: t -*-
;; scenario-display: graphic

;;; Commentary:
;; Plan 094, step 2.  Each row opens a copy of a fixture of
;; `tests/golden/', reindents the whole buffer with C-x h C-M-\, and
;; checks with `:golden' (`tests/helpers/outcomes/golden.el') that
;; reindenting changed nothing and that every token has the face that
;; `tests/golden/NAME.faces' approves.  Package updates are the usual
;; source of such regressions.  After reviewing a failure, approve an
;; intended change with
;;
;;   UPDATE_GOLDEN=1 tests/run fast golden
;;
;; and commit the files of `tests/golden/' it rewrote.
;;
;; The record holds face attributes, such as the italic of comments,
;; as a graphical frame shows them: the theme's face specifications
;; give a terminal frame other ones.  Hence the graphical display.

;;; Code:

(require 'scenario)

(scenario
 :describe "GOLDEN: reindenting a fixture changes nothing, and each token keeps its approved face"
 :given ((:name "emacs-lisp" :project "../golden" :entry "sample.el")
         (:name "python" :project "../golden" :entry "sample.py")
         (:name "clojure" :project "../golden" :entry "sample.clj")
         (:name "org" :project "../golden" :entry "sample.org")
         (:name "markdown" :project "../golden" :entry "sample.md")
         (:name "shell" :project "../golden" :entry "sample.sh")
         (:name "nix" :project "../golden" :entry "sample.nix"))
 :when ((:keys "C-x h C-M-\\"))
 :then ((:golden)))

(provide 'l1-golden-gui)

;;; l1-golden-gui.el ends here
