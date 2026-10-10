;;; ledger.el --- The known-defects ledger of the integrity checks -*- lexical-binding: t -*-

;;; Commentary:
;; Known defects live in `tests/integrity/known-defects/NNN.el', one
;; file per owning plan NNN.  Each file holds one quoted list of
;; entries (CHECK SUBJECT ID084 REASON):
;; - CHECK, a symbol naming the check, for example `dangling-binding';
;; - SUBJECT, a string naming what the check reports, never a line
;;   number, for example "global C-c n u";
;; - ID084, the identifier of the finding in review 084, for example
;;   "CF-09";
;; - REASON, a string.
;; A listed defect is tolerated, an unlisted one fails its check, and a
;; listed one that no longer occurs fails as stale.

;;; Code:

(require 'seq)

(defun my/integrity-ledger-entries (directory)
  "Return the entries of every ledger file in DIRECTORY.
The files are the `NNN.el' files, read in name order; only the
first form of each is read.  Signal an error naming the file when
that form is not a quoted list of well-formed entries."
  (let (entries)
    (dolist (file (directory-files directory t "\\`[0-9][0-9][0-9]\\.el\\'"))
      (let ((read (with-temp-buffer
                    (insert-file-contents file)
                    (condition-case err
                        (read (current-buffer))
                      (error (error "%s: %s" file (error-message-string err)))))))
        (unless (and (eq (car-safe read) 'quote) (proper-list-p (cadr read)))
          (error "%s: not a quoted list" file))
        (dolist (entry (cadr read))
          (pcase entry
            (`(,(pred symbolp) ,(pred stringp)
               ,(and (pred stringp) (pred (string-match-p "\\`[A-Z]+-[0-9]+\\'")))
               ,(pred stringp))
             (push entry entries))
            (_ (error "%s: malformed entry %S" file entry))))))
    (nreverse entries)))

(defun my/integrity-ledger-compare (check subjects entries)
  "Compare the SUBJECTS that CHECK found with the ledger ENTRIES.
Return (:unexpected UNEXPECTED :stale STALE): UNEXPECTED are the
SUBJECTS no entry for CHECK lists, STALE the subjects listed for
CHECK that are not in SUBJECTS.  The check passes when both are
nil."
  (let ((listed (mapcar #'cadr (seq-filter (lambda (entry) (eq (car entry) check))
                                           entries))))
    (list :unexpected (seq-uniq (seq-difference subjects listed))
          :stale (seq-difference listed subjects))))

(provide 'ledger)
;;; ledger.el ends here
