;;; l1-journey-10-corfu-popup.el --- Journey 10: typing a prefix pops up completions -*- lexical-binding: t -*-
;; scenario-display: graphic

;;; Commentary:
;; Plan 094, journey 10, and U05 of the 085 mutation baseline.  In an
;; Emacs Lisp buffer the user types the start of a symbol and pauses:
;; Corfu's popup appears by itself (`corfu-auto') and lists the
;; matching symbols, shortest first; <return> inserts the first one.
;; Without `corfu-auto', the mutation, no popup appears and <return>
;; breaks the line.  The popup is a child frame, so the file needs a
;; graphical frame.

;;; Code:

(require 'scenario)

(scenario
 :describe "J-10: a typed prefix and a pause show the completion popup"
 :given ((:name "elisp" :file "sample.el" :text ";; sample\n-!-"))
 :when ((:type "(with-current-buf") (:idle 400))
 :then ((:completion-popup-shows "\\`[^\n]*with-current-buffer *\n")))

(scenario
 :describe "J-10: <return> inserts the first candidate of the popup"
 :given ((:name "elisp" :file "sample.el" :text ";; sample\n-!-"))
 :when ((:type "(with-current-buf") (:idle 400) (:keys "<return>"))
 :then ((:buffer-text ";; sample\n(with-current-buffer-!-)")))

(provide 'l1-journey-10-corfu-popup)

;;; l1-journey-10-corfu-popup.el ends here
