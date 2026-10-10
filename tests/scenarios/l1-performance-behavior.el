;;; l1-performance-behavior.el --- Performance correctness checks -*- lexical-binding: t; -*-
;; scenario-display: graphic

;;; Commentary:
;; Plan 036 defers fontification while input is pending
;; (`jit-lock-defer-time' 0): text that Emacs redisplays while the user
;; types ahead stays unhighlighted until the user pauses, when
;; jit-lock's idle timer fontifies it.  The scenario types ahead of
;; redisplay, pauses, and checks the face redisplay then shows; only a
;; graphical frame redisplays.  The dashboard refresh check lives in
;; `l1-dashboard-gui.el', with the client frame that shows it.

;;; Code:

(require 'scenario)

(scenario
 :describe "Keyword highlighting recovers once the user pauses after typing ahead"
 :given ((:name "elisp" :file "sample.el" :text "(-!-defun example () nil)\n"))
 :when ((:type "x") (:keys "<backspace>") (:redisplay-while-typing) (:idle 200))
 :then ((:buffer-text "(-!-defun example () nil)\n")
        (:face-shown font-lock-keyword-face)))

(provide 'l1-performance-behavior)

;;; l1-performance-behavior.el ends here
