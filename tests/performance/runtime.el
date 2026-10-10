;;; runtime.el --- Graphical editing and allocation probes -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(defvar perf-runtime-files nil)
(defvar perf-runtime-results nil)
(defvar perf-runtime-index 0)
(defvar perf-runtime-end nil)
(defvar perf-runtime-output nil)
(defvar perf-runtime-timer nil)
(defvar perf-runtime-profile nil)
(defvar perf-runtime-heartbeat nil)
(defvar perf-runtime-last-beat nil)
(defvar perf-runtime-lag-max 0.0)
(defvar perf-runtime-lag-counts [0 0 0 0 0 0 0 0])
(defvar perf-runtime-interval 0.5)
(defvar perf-runtime-profile-delay 0)

(defun perf-runtime-beat ()
  "Measure command-loop gaps, including time blocked in process filters."
  (let* ((now (float-time))
         (lag (max 0.0 (- now perf-runtime-last-beat 0.02)))
         (bounds '(0.001 0.004 0.008 0.0167 0.05 0.1 0.5))
         (bucket 0))
    (while (and bounds (> lag (pop bounds))) (cl-incf bucket))
    (cl-incf (aref perf-runtime-lag-counts bucket))
    (setq perf-runtime-lag-max (max lag perf-runtime-lag-max)
          perf-runtime-last-beat now)))

(defun perf-runtime-exit-log ()
  "Record the last workload state if the test editor exits."
  (when perf-runtime-output
    (with-temp-file (concat perf-runtime-output ".exit")
      (let ((standard-output (current-buffer)))
        (prin1 (list :index perf-runtime-index :remaining (- perf-runtime-end (float-time))))
        (backtrace)))))
(add-hook 'kill-emacs-hook #'perf-runtime-exit-log)

(defun perf-runtime-sample (file)
  "Exercise real mode hooks and commands in disposable FILE."
  (let* ((open-start (float-time))
         (existing (get-file-buffer file))
         (buffer (find-file-noselect file))
         (open-seconds (- (float-time) open-start))
         samples errors)
    (switch-to-buffer buffer)
    (goto-char (point-min))
    (forward-line 8)
    (end-of-line)
    (dolist (key '("x" "DEL" "C-n" "C-p" "C-v" "M-v" "C-s" "C-g"))
      (let ((start (float-time)) (before-gc gc-elapsed))
        (condition-case error
            (progn (execute-kbd-macro (kbd key)) (redisplay t))
          (quit nil)
          (error (push (error-message-string error) errors)))
        (push (vector key (- (float-time) start) (- gc-elapsed before-gc)) samples)))
    ;; Only the disposable file is touched; undo the sample's insertion/deletion.
    (set-buffer-modified-p nil)
    `((epoch . ,(float-time))
      (file . ,(file-name-nondirectory file))
      (mode . ,(symbol-name major-mode))
      (first_open . ,(if existing :false t))
      (open_seconds . ,open-seconds)
      (samples . ,(vconcat (nreverse samples)))
      (errors . ,(vconcat errors))
      (lsp . ,(if (bound-and-true-p lsp-mode) t :false))
      (workspaces . ,(vconcat
                      (when (bound-and-true-p lsp-mode)
                        (mapcar (lambda (workspace)
                                  (vector (format "%s" (lsp--workspace-server-id workspace))
                                          (format "%s" (lsp--workspace-status workspace))
                                          (lsp--workspace-root workspace)))
                                (lsp-workspaces)))))
      (gc_seconds . ,gc-elapsed)
      (gc_count . ,gcs-done)
      (loop_lag_max . ,perf-runtime-lag-max)
      (loop_lag_histogram . ,(copy-sequence perf-runtime-lag-counts))
      (rss_kb . ,(or (cdr (assq 'rss (process-attributes (emacs-pid)))) 0)))))

(defun perf-runtime-write ()
  "Append the latest measurement without retaining an expanding history."
  (let ((record (pop perf-runtime-results)))
    (with-temp-buffer
      (insert (json-serialize record) "\n")
      (write-region (point-min) (point-max) perf-runtime-output t 'silent))))

(defun perf-runtime-tick ()
  "Run one short editing sample, allowing timers and process filters between."
  (if (> (float-time) perf-runtime-end)
      (progn
        (cancel-timer perf-runtime-timer)
        (cancel-timer perf-runtime-heartbeat)
        (when perf-runtime-profile
          (profiler-stop)
          (let ((coding-system-for-write 'utf-8-unix)
                (print-escape-nonascii t)
                (print-escape-multibyte t)
                (print-escape-control-characters t))
            (profiler-write-profile (profiler-cpu-profile)
                                    (concat perf-runtime-output ".cpu.el"))
            (profiler-write-profile (profiler-memory-profile)
                                    (concat perf-runtime-output ".memory.el"))))
        (with-temp-file (concat perf-runtime-output ".done")
          (insert (format "%d samples completed\n" perf-runtime-index))))
    (condition-case error
        (push (perf-runtime-sample
               (nth (% perf-runtime-index (length perf-runtime-files)) perf-runtime-files))
              perf-runtime-results)
      (error
       (push `((error . ,(error-message-string error))) perf-runtime-results)))
    (perf-runtime-write)
    (setq perf-runtime-lag-max 0.0 perf-runtime-lag-counts (make-vector 8 0))
    (cl-incf perf-runtime-index)))

(defun perf-runtime-start (files output seconds &optional profile)
  "Exercise FILES for SECONDS, writing JSON lines to OUTPUT.
PROFILE enables CPU sampling for attribution, not acceptance timings."
  (setq perf-runtime-files files
        perf-runtime-output output
        perf-runtime-end (+ (float-time) seconds)
        perf-runtime-index 0
        perf-runtime-profile profile
        debug-on-error nil)
  (when profile
    (require 'profiler)
    (run-at-time perf-runtime-profile-delay nil
                 (lambda () (profiler-start 'cpu+mem))))
  (setq perf-runtime-last-beat (float-time)
        perf-runtime-heartbeat (run-at-time .02 .02 #'perf-runtime-beat))
  (setq perf-runtime-timer (run-at-time 0 perf-runtime-interval #'perf-runtime-tick))
  "started")

(provide 'perf-runtime)
