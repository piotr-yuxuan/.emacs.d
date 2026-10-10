;;; diff-hl-test.el --- Logic tests of diff-hl's revision copies -*- lexical-binding: t -*-

;;; Commentary:
;; Logic tests: the name diff-hl gives the copy of a revision it diffs
;; an unsaved buffer against, through `diff-hl-make-temp-file-name' as
;; the configuration advises it.  The name is computed, never written,
;; so no stub is needed.

;;; Code:

(require 'buttercup)
(require 'diff-hl)

(defconst diff-hl-test--deep-file
  (concat "/srv/shared (team)/src/forge.example.org/team/widgets/"
          "releases/2026-09-16 release candidate, second review/"
          "document=attachments/system-architecture-exercise/"
          "output=second-draft/review=after-the-first-reading/"
          "notes=from-the-reviewers/proposed-architecture.org")
  "A file whose path alone is longer than a file name may be.")

(defconst diff-hl-test--revision "0123456789abcdef0123456789abcdef01234567"
  "A Git commit, as diff-hl passes it.")

(defun diff-hl-test--copy (file revision &optional manual)
  "Return the name diff-hl gives its copy of REVISION of FILE.
MANUAL is passed through: diff-hl names an automatic and a manual
copy of the same revision."
  (let ((temporary-file-directory "/dev/shm/"))
    (diff-hl-make-temp-file-name file revision manual)))

(describe "The copy of a revision diff-hl diffs against"
  (it "has a name, and a lock file name, that fit in a file name"
    (expect (string-bytes diff-hl-test--deep-file) :to-be-greater-than 255)
    (dolist (manual '(nil manual))
      (let ((copy (diff-hl-test--copy diff-hl-test--deep-file
                                      diff-hl-test--revision manual)))
        (expect (string-bytes (file-name-nondirectory copy))
                :not :to-be-greater-than 255)
        (expect (string-bytes (file-name-nondirectory (make-lock-file-name copy)))
                :not :to-be-greater-than 255)
        (expect (file-name-directory copy) :to-equal "/dev/shm/"))))

  (it "differs between files, revisions, and automatic and manual copies"
    (let ((names (list (diff-hl-test--copy diff-hl-test--deep-file
                                           diff-hl-test--revision 'manual)
                       (diff-hl-test--copy diff-hl-test--deep-file
                                           diff-hl-test--revision)
                       (diff-hl-test--copy diff-hl-test--deep-file
                                           ";0123456789abcdef" 'manual)
                       ;; Another clone of the same repository: only the
                       ;; head of the path differs.
                       (diff-hl-test--copy (concat "/srv/clone" diff-hl-test--deep-file)
                                           diff-hl-test--revision 'manual)
                       (diff-hl-test--copy "/srv/notes.org"
                                           diff-hl-test--revision 'manual))))
      (expect (length (delete-dups (copy-sequence names))) :to-equal 5))))

(provide 'diff-hl-test)

;;; diff-hl-test.el ends here
