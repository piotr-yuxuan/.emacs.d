;;; treemacs-probe.el --- Log each read of a string that ends early -*- lexical-binding: t -*-

;;; Commentary:
;; Plan 119, step 1b.  Temporary: plan 119 removes it once it knows
;; which `read' fails.  Workflow treemacs-probe.yml appends it to the
;; init.el of the clone it tests, never to the repository's file.  Each
;; `read' of a string that signals `end-of-file' appends its time, its
;; argument and the backtrace of its callers to ~/read-eof.log, then
;; signals again.

;;; Code:

(require 'backtrace)

(defun my/probe-read-eof (read &rest args)
  "Call READ with ARGS; log an `end-of-file' on a string, then signal it."
  (condition-case err
      (apply read args)
    (end-of-file
     (when (stringp (car args))
       (let ((callers (backtrace-to-string)))
         (with-temp-buffer
           (insert (format "=== %s, emacs pid %d, argument of %d characters: %S\n"
                           (format-time-string "%T.%3N") (emacs-pid)
                           (length (car args)) (truncate-string-to-width (car args) 400))
                   callers "\n")
         (write-region (point-min) (point-max) "~/read-eof.log" t 'silent))))
     (signal (car err) (cdr err)))))

(advice-add 'read :around #'my/probe-read-eof)

;;; treemacs-probe.el ends here
