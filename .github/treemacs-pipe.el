;;; treemacs-pipe.el --- Start pfuture's processes on a pipe -*- lexical-binding: t -*-

;;; Commentary:
;; Plan 119, step 1d, second half of the probe: the candidate fix,
;; appended to the clone's init.el after treemacs-probe.el.  Temporary.

;;; Code:

(define-advice pfuture-new (:around (new &rest command) my/probe-pipe)
  (let ((process-connection-type nil))
    (apply new command)))

;;; treemacs-pipe.el ends here
