;;; pty-probe.el --- Does a pty cut the output of Treemacs's helper? -*- lexical-binding: t -*-

;;; Commentary:
;; Plan 119, step 1.  Temporary: plan 119 removes it once its decision
;; rests on what it printed.  Runs Treemacs's collapse helper RUNS
;; times through pfuture, as Treemacs does, first with a pty and then
;; with a pipe, and prints one JSON line for each: how many outputs
;; `read' could not parse.
;;
;; emacs --batch -l pty-probe.el PFUTURE-DIR TREEMACS-DIR ROOT RUNS

;;; Code:

(require 'cl-lib)

(let* ((args command-line-args-left)
       (script (expand-file-name "src/scripts/treemacs-dirs-to-collapse.py"
                                 (nth 1 args)))
       (root (directory-file-name (expand-file-name (nth 2 args))))
       (roots (cons root (mapcar (lambda (dir) (concat root "/" dir))
                                 '("lib" "src" "test"))))
       (runs (string-to-number (nth 3 args))))
  (setq command-line-args-left nil)
  (add-to-list 'load-path (expand-file-name (nth 0 args)))
  (require 'pfuture)
  (dolist (type '(pty pipe))
    (let ((process-connection-type (eq type 'pty))
          (failed 0)
          (unreadable 0)
          (lengths nil))
      (dotimes (_ runs)
        (let* ((future (apply #'pfuture-new "python3" "-O" script "5" "x" roots))
               (output (pfuture-await-to-finish future)))
          (push (length output) lengths)
          (if (/= 0 (process-exit-status future))
              (cl-incf failed)
            (condition-case nil
                (read output)
              (error (cl-incf unreadable))))))
      (princ (format (concat "{\"system\": \"%s\", \"emacs\": \"%s\", \"type\": \"%s\","
                             " \"runs\": %d, \"failed\": %d, \"unreadable\": %d,"
                             " \"shortest\": %d, \"longest\": %d}\n")
                     system-type emacs-version type runs failed unreadable
                     (apply #'min lengths) (apply #'max lengths))))))

;;; pty-probe.el ends here
