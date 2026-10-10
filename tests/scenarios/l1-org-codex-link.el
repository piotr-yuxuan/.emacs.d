;;; l1-org-codex-link.el --- Open Codex thread links from Org -*- lexical-binding: t; -*-

;;; Commentary:
;; Plan 078.  Following a codex:// link with Org's standard C-c C-o
;; command must preserve the complete URL passed to `browse-url'.  The
;; desktop browser is the system boundary, so the row records it.

;;; Code:

(require 'scenario)

(scenario
 :describe "BL1-ORG-CODEX-LINK: C-c C-o opens a Codex thread URL"
 :given ((:name "described Codex link"
          :file "conversation.org"
          :text "This [[codex://threads/01234567-89ab-cdef-0123-456789abcdef][Codex conversa-!-tion]] talks about..."
          :system (:record-browse-url t)))
 :when ((:keys "C-c C-o"))
 :then ((:browsed-url "codex://threads/01234567-89ab-cdef-0123-456789abcdef")))

(provide 'l1-org-codex-link)

;;; l1-org-codex-link.el ends here
