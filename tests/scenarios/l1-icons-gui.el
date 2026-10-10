;;; l1-icons-gui.el --- Icons in the mode line and in completions -*- lexical-binding: t -*-
;; scenario-display: graphic

;;; Commentary:
;; 084 DD-05: icons were decided when the configuration loaded, with
;; `display-graphic-p', which is nil while a daemon starts: the client
;; frames of a daemon then showed no icon in the mode line.  The
;; completion icons depended on nerd-icons loading before
;; marginalia-mode started.  Both are checked in a client frame of a
;; daemon, as the user starts Emacs.  An icon is a character of the
;; private use areas, where the Nerd Fonts put their glyphs.

;;; Code:

(require 'scenario)

(scenario
 :describe "DD-05: the mode line of a daemon's client frame shows the major mode icon"
 :given ((:name "elisp" :file "notes.el" :text ";; notes-!-\n"))
 :when ((:open))
 :then ((:mode-line-matches "[-\U000f0000-\U000ffffd]")))

(scenario
 :describe "DD-05: M-x candidates show their icons"
 :given ((:name "M-x" :file "notes.el" :text ";; notes-!-\n"))
 :when ((:keys "M-x") (:type "find-fil") (:sample "candidates") (:keys "C-g"))
 :then ((:sampled "candidates" :minibuffer "[-\U000f0000-\U000ffffd] *find-file")))

(provide 'l1-icons-gui)

;;; l1-icons-gui.el ends here
