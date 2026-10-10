;;; cheatsheet.el --- Write docs/reference/keys.org from the live keymaps -*- lexical-binding: t -*-

;;; Commentary:
;; Run by `tests/integrity/session/run cheatsheet' in a daemon session
;; of 085's runner, like `t1.el'.  It loads every configured feature as
;; T1 does, then writes the cheatsheet of the keymaps the configuration
;; binds keys in (`my/t1-cheatsheet').  The output depends only on the
;; configuration and the installed packages.

;;; Code:

(require 'buttercup)
(let ((directory (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path directory)
  (add-to-list 'load-path (expand-file-name ".." directory)))
(require 't1-checks)

(describe "The key cheatsheet"
  (it "is written from the live keymaps"
    (buttercup-suppress-warning-capture
      (let* ((files (mapcar (lambda (name)
                              (cons name (my/integrity-forms
                                          (expand-file-name name my/t1-root))))
                            '("early-init.el" "init.el")))
             (forms (apply #'append (mapcar #'cdr files)))
             (coding-system-for-write 'utf-8-unix))
        (my/t1-force-load forms)
        (make-directory (expand-file-name "docs/reference" my/t1-root) t)
        (write-region (my/t1-cheatsheet
                       (my/integrity-bindings forms)
                       (mapcar (lambda (file)
                                 (cons (car file) (my/integrity-unresolved (cdr file))))
                               files))
                      nil (expand-file-name "docs/reference/keys.org" my/t1-root)
                      nil 'silent)))))

;;; cheatsheet.el ends here
