;;; outcomes.el --- Load every outcome definition -*- lexical-binding: t -*-

;;; Commentary:
;; Requires each file of `tests/helpers/outcomes/' except the `*-test.el'
;; self-tests, in name order.  A plan adds its outcomes by adding its
;; own file there; no shared file needs an edit.

;;; Code:

(dolist (file (directory-files
               (expand-file-name "outcomes/"
                                 (file-name-directory
                                  (or load-file-name buffer-file-name)))
               nil "\\`[^.].*\\.el\\'"))
  (unless (string-suffix-p "-test.el" file)
    (require (intern (concat "outcomes/" (file-name-sans-extension file))))))

(provide 'outcomes)

;;; outcomes.el ends here
