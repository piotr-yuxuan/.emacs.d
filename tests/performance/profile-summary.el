;;; profile-summary.el --- Inclusive profiler attribution -*- lexical-binding: t; -*-

(require 'profiler)
(require 'cl-lib)

(dolist (path command-line-args-left)
  (let ((counts (make-hash-table :test 'equal)) rows)
    (maphash
     (lambda (stack weight)
       ;; Count each named function once per stack. Inclusive counts overlap;
       ;; they identify attribution, not additive portions of wall time.
       (dolist (function (delete-dups (append stack nil)))
         (when (and function (symbolp function))
           (cl-incf (gethash (symbol-name function) counts 0) weight))))
     (profiler-profile-log (profiler-read-profile path)))
    (maphash (lambda (name count) (push (cons name count) rows)) counts)
    (princ (format "%s\n" path))
    (dolist (row (seq-take (sort rows (lambda (a b) (> (cdr a) (cdr b)))) 40))
      (princ (format "%10d %s\n" (cdr row) (car row))))))
