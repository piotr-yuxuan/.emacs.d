;;; post-workload.el --- Explicit GC diagnostic after acceptance samples -*- lexical-binding: t; -*-

(defun perf-post-workload-gc (&rest _)
  "Record live objects and RSS after the timed workload has ended.
This explicit collection is outside the acceptance sample boundary."
  (when (and perf-runtime-output
             (file-exists-p (concat perf-runtime-output ".done")))
    (advice-remove 'perf-runtime-tick #'perf-post-workload-gc)
    (let* ((before (cdr (assq 'rss (process-attributes (emacs-pid)))))
           (start (float-time))
           (objects (garbage-collect))
           (seconds (- (float-time) start))
           (after (cdr (assq 'rss (process-attributes (emacs-pid))))))
      (with-temp-file (concat perf-runtime-output ".post-gc.json")
        (insert (json-serialize
                 `((rss_before_kb . ,before)
                   (rss_after_kb . ,after)
                   (gc_seconds . ,seconds)
                   (live_objects . ,(format "%S" objects)))))))))

(advice-add 'perf-runtime-tick :after #'perf-post-workload-gc)
