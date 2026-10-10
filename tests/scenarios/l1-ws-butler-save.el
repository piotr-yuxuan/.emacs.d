;;; l1-ws-butler-save.el --- Plan 071: format-on-save trailing-whitespace stripping -*- lexical-binding: t -*-

;;; Commentary:
;; Plan 071 / ws-butler slice.  The user opens a file, types a new
;; line that ends with trailing whitespace, moves point off the line
;; (the live config keeps `ws-butler-keep-whitespace-before-point' at
;; its default t, so whitespace before point survives), then saves
;; with C-x C-s.  ws-butler strips trailing whitespace from the lines
;; edited in this session.

;;; Code:

(require 'scenario)

(scenario
 :describe "BL1-WS-BUTLER-SAVE: typed trailing whitespace is stripped on save (ws-butler)"
 :given ((:name "el"
          :file "scratch.el"
          :text ";; test buffer\n-!-")
         (:name "sh"
          :file "scratch.sh"
          :text "echo a\n-!-")
         (:name "py"
          :file "scratch.py"
          :text "x = 1\n-!-")
         (:name "txt"
          :file "scratch.txt"
          :text "first line\n-!-"))
 :when ((:type "newline   ") (:keys "C-<home> C-x C-s"))
 :then ((:no-trailing-whitespace)))

(provide 'l1-ws-butler-save)

;;; l1-ws-butler-save.el ends here
