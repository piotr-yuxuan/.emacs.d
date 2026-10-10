;;; check-pins.el --- Each installed package repository sits at its pin -*- lexical-binding: t -*-

;;; Commentary:
;; Run by the CI workflow from the root of the configuration, once
;; straight.el has installed the packages there:
;;
;;   emacs --batch --load .github/check-pins.el
;;
;; Every repository under straight/repos/ must be checked out at the
;; commit that the lock file, straight/versions/default.el, records
;; for it.  A repository without a pin, or at another commit, means
;; that a fresh clone does not get the packages the tests ran with.
;; The exit status is not 0 then, and the message names each such
;; repository.

;;; Code:

(let ((pins (with-temp-buffer
              (insert-file-contents "straight/versions/default.el")
              (read (current-buffer))))
      (repositories (directory-files "straight/repos" nil directory-files-no-dot-files-regexp))
      (off-pin nil))
  (unless repositories
    (error "No repository is installed under straight/repos"))
  (dolist (name repositories)
    (let ((head (car (process-lines "git" "-C" (expand-file-name name "straight/repos")
                                    "rev-parse" "HEAD")))
          (pin (cdr (assoc name pins))))
      (unless (equal head pin)
        (push (format "%s is at %s, its pin is %s" name head (or pin "missing")) off-pin))))
  (when off-pin
    (error "%d of %d repositories do not sit at their pin:\n%s"
           (length off-pin) (length repositories) (string-join (nreverse off-pin) "\n")))
  (message "%d repositories sit at their pin" (length repositories)))

;;; check-pins.el ends here
