;;; l1-window-fkey-dispatch.el --- File-tree toggle and Imenu fallback -*- lexical-binding: t -*-

;;; Commentary:
;; F8 toggles the Treemacs file tree; on the first use of an empty
;; workspace, Treemacs asks for its root directory.  F9 uses LSP
;; Treemacs in managed buffers and Imenu otherwise.  This file presses
;; F8 and the Imenu fallback of F9.  Python LSP dispatch of F9 lives in
;; l3-python-intelligence.el.  The F8 scenario used to add the project
;; to the workspace with Lisp before pressing F8; it now answers the
;; prompt, as the user does.

;;; Code:

(require 'scenario)

(scenario
 :describe "BL1-FKEY-F8: F8 asks for a root on first use, opens the tree, then closes it"
 ;; The first F8 of a session meets an empty Treemacs workspace, so this
 ;; scenario stays the first of the file.
 :given ((:name "file tree" :project "python-intelligence" :entry "pyproject.toml"))
 :when ((:keys "<f8>") (:sample "prompt") (:keys "<return>") (:sample "open")
        (:keys "<f8>"))
 :then ((:sampled "prompt" :minibuffer "Project root: .*/python-intelligence/")
        (:sampled "open" :buffer "Treemacs-Buffer")
        (:windows-count 1)
        (:file-suffix "python-intelligence/pyproject.toml")))

(scenario
 :describe "BL1-FKEY-F9: <f9> shows the Imenu list outside LSP buffers"
 :given ((:name "elisp-buffer"
          :file "scratch.el"
          :text "(defun f (x) x)\n"))
 :when ((:keys "<f9>"))
 ;; `my/symbol-outline-toggle' runs `imenu-list-smart-toggle' when
 ;; `lsp-managed-mode' is off.
 :then ((:window-shows "\\`\\*Ilist\\*\\'")))

(provide 'l1-window-fkey-dispatch)

;;; l1-window-fkey-dispatch.el ends here
