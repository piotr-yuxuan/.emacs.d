;;; l3-java-first-file.el --- The first Java file of a session starts LSP -*- lexical-binding: t -*-
;; scenario-tier: l3

;;; Commentary:
;; 084 CF-15: lsp-java's hooks waited for lsp-mode, so a Java file
;; opened before any other language server had started got no LSP.
;; This file is the only row of its session, so lsp-mode is not loaded
;; when the Java file opens.  Once the user is idle, lsp-mode starts and,
;; as the sandbox is no project of the private lsp session yet, asks
;; whether to import it; n declines, so no server process starts.  The
;; prompt is what the user sees of LSP starting for the buffer; without
;; it, n would be typed into the file.
;;
;; lsp-mode asks only when the server it would run is installed, here
;; the jdtls on PATH, so this file is L3.  On a host without jdtls it
;; fails rather than skips: 085's harness skips only on the servers of
;; `my/scenario-lsp-servers', which has no jdtls.

;;; Code:

(require 'scenario)

(scenario
 :describe "CF-15: the first Java file of a session starts LSP"
 :given ((:name "first Java file" :file "src/Main.java" :text "class Main {}-!-\n"))
 :when ((:idle 500) (:sample "prompt") (:keys "n"))
 :then ((:sampled "prompt" :minibuffer "Main\\.java is not part of any project")
        (:buffer-text-unchanged)))

(provide 'l3-java-first-file)

;;; l3-java-first-file.el ends here
