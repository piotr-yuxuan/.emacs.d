;;; treemacs-probe.el --- Record the state of a future Treemacs fails to parse -*- lexical-binding: t -*-

;;; Commentary:
;; Plan 119, step 1d.  Temporary: plan 119 removes it once it knows why
;; the collapse helper's output reads as empty.  Workflow
;; treemacs-probe.yml appends it to the init.el of the clone it tests,
;; never to the repository's file.  When `treemacs--parse-flattened-dirs'
;; signals, it appends to ~/read-eof.log the state of the future before
;; and after the parse: status, exit status, the output pfuture kept,
;; and what pfuture's sentinel saw if it ran.

;;; Code:

(defun my/probe-state (future)
  "Return a plist of what can be told of FUTURE now."
  (let ((buffer (process-get future 'buffer)))
    (list :elapsed (- (float-time) (or (process-get future 'probe-start) 0))
          :status (process-status future)
          :exit (process-exit-status future)
          :tty (process-get future 'probe-tty)
          :result (process-get future 'result)
          :buffer-chars (and (buffer-live-p buffer) (buffer-size buffer))
          :stderr (process-get future 'stderr)
          :sentinel (process-get future 'probe-sentinel))))

(define-advice pfuture-new (:filter-return (future) my/probe-start)
  (process-put future 'probe-start (float-time))
  (process-put future 'probe-tty (process-tty-name future))
  future)

(define-advice pfuture--sentinel (:before (future event) my/probe-sentinel)
  (process-put future 'probe-sentinel
               (list :event event :state (my/probe-state future))))

(define-advice treemacs--parse-flattened-dirs (:around (parse path future) my/probe-parse)
  (let ((before (and future (my/probe-state future))))
    (condition-case err
        (funcall parse path future)
      (error
       (with-temp-buffer
         (insert (format "=== %s pid %d %S\nbefore: %S\nafter: %S\n\n"
                         (format-time-string "%T.%3N") (emacs-pid) err
                         before (my/probe-state future)))
         (write-region (point-min) (point-max) "~/read-eof.log" t 'silent))
       (signal (car err) (cdr err))))))

;;; treemacs-probe.el ends here
