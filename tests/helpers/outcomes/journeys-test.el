;;; outcomes/journeys-test.el --- Self-tests of the journey outcomes -*- lexical-binding: t -*-

;;; Commentary:
;; Each outcome must hold on the fact it names and fail, saying what
;; the user sees instead, when the fact is missing.  The positive case
;; of `:completion-popup-shows' needs a graphical frame; journey 10
;; (tests/scenarios/l1-journey-10-corfu-popup.el) covers it.

;;; Code:

(require 'buttercup)
(require 'scenario/outcome)
(require 'outcomes)

(defun outcomes-journeys-test--verdict (clause &optional context)
  "Return the verdict of CLAUSE with CONTEXT."
  (my/scenario-check-clause clause)
  (my/scenario-outcome-verdict clause context))

(describe "Journey outcomes"

  (it ":saved-file-matches reads the file on disk, under the row's repository"
    (let* ((root (file-name-as-directory (make-temp-file "outcomes-journeys-" t)))
           (entry (generate-new-buffer " outcomes-journeys-entry")))
      (unwind-protect
          (progn
            (make-directory (expand-file-name ".git" root))
            (make-directory (expand-file-name "src" root))
            (write-region "saved text\n" nil (expand-file-name "src/a.txt" root) nil 'silent)
            (with-current-buffer entry
              (setq default-directory (expand-file-name "src/" root))
              ;; Unsaved text in a buffer does not count.
              (insert "unsaved text"))
            (let ((context (list :entry-buffer entry)))
              (expect (outcomes-journeys-test--verdict
                       '(:saved-file-matches "src/a.txt" "\\`saved text\n\\'") context)
                      :to-be nil)
              (expect (outcomes-journeys-test--verdict
                       '(:saved-file-matches "src/a.txt" "unsaved") context)
                      :to-match "holds \"saved text")
              (expect (outcomes-journeys-test--verdict
                       '(:saved-file-matches "src/missing.txt" "x") context)
                      :to-match "not readable"))
            (kill-buffer entry)
            (expect (outcomes-journeys-test--verdict
                     '(:saved-file-matches "src/a.txt" "saved") (list :entry-buffer entry))
                    :to-match "entry buffer is gone"))
        (when (buffer-live-p entry) (kill-buffer entry))
        (delete-directory root t))))

  (it ":buffer-named-matches checks the text of another buffer"
    (let ((buffer (get-buffer-create "outcomes-journeys-other")))
      (unwind-protect
          (progn
            (with-current-buffer buffer (insert "* Inbox\n** TODO Buy milk\n"))
            (with-temp-buffer
              (expect (outcomes-journeys-test--verdict
                       '(:buffer-named-matches "\\`outcomes-journeys-other\\'"
                                               "^\\*\\* TODO Buy milk$"))
                      :to-be nil)
              (expect (outcomes-journeys-test--verdict
                       '(:buffer-named-matches "\\`outcomes-journeys-other\\'" "Projects"))
                      :to-match "holds \"\\* Inbox")
              (expect (outcomes-journeys-test--verdict
                       '(:buffer-named-matches "\\`outcomes-journeys-absent\\'" "x"))
                      :to-match "no buffer name matches")))
        (kill-buffer buffer))))

  (it ":completion-popup-shows fails without a visible popup"
    (expect (outcomes-journeys-test--verdict '(:completion-popup-shows "x"))
            :to-equal "no completion popup is visible")))

(provide 'outcomes/journeys-test)

;;; outcomes/journeys-test.el ends here
