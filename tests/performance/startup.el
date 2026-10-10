;;; startup.el --- Opt-in startup instrumentation -*- lexical-binding: t; -*-

(defvar perf-startup-stages nil)
(defvar perf-startup-loads nil)
(defvar perf-startup-gcs gcs-done)
(defvar perf-dashboard-renders 0)
(add-hook 'dashboard-before-initialize-hook (lambda () (cl-incf perf-dashboard-renders)))
(setq debug-on-error t
      debugger (lambda (&rest args)
                 (let ((standard-output 'external-debugging-output))
                   (prin1 args) (backtrace))
                 (kill-emacs 2)))

(defun perf-mark (name)
  "Record startup stage NAME without performing file I/O."
  (push (cons name (float-time)) perf-startup-stages))

(defun perf-time-load (function &rest args)
  "Time a FUNCTION call with ARGS during an explicit profile."
  (let ((start (float-time)))
    (prog1 (apply function args)
      (push (list (format "%s" (car args)) (- (float-time) start))
            perf-startup-loads))))

(defun perf-ready-command ()
  "Record completion of a command and redisplay in the graphical frame."
  (interactive)
  (redisplay t)
  (perf-mark 'ready)
  (let ((report
         `((stages . ,(reverse perf-startup-stages))
           (init_seconds . ,(float-time (time-subtract after-init-time before-init-time)))
           (gcs . ,(- gcs-done perf-startup-gcs))
           (gc_seconds . ,gc-elapsed)
           (gc_threshold . ,gc-cons-threshold)
           (dashboard_renders . ,perf-dashboard-renders)
           (buffer . ,(buffer-name))
           (mode . ,(symbol-name major-mode))
           (window_system . ,(symbol-name window-system))
           (frame_pixels . ,(vector (frame-pixel-width) (frame-pixel-height)))
           (frame_visible . ,(if (eq (frame-visible-p (selected-frame)) t) t :false))
           (daemon . ,(if (daemonp) t :false))
           (loads . ,(vconcat (mapcar #'vconcat perf-startup-loads))))))
    (with-temp-file (getenv "EMACS_PERF_READY")
      (insert (json-serialize report))))
  (when (getenv "EMACS_PERF_EXIT")
    (kill-emacs 0))
  (setq debug-on-error nil debugger #'debug))

(defun perf-enqueue-ready ()
  "Measure the next command-loop turn after startup hooks finish."
  (perf-mark 'window_setup)
  (global-set-key [perf-ready] #'perf-ready-command)
  (setq unread-command-events
        (append unread-command-events (list 'perf-ready))))

(add-hook 'after-init-hook (lambda () (perf-mark 'after_init_hook)) 100)
(add-hook 'emacs-startup-hook (lambda () (perf-mark 'startup_hook)) 100)
(add-hook 'window-setup-hook #'perf-enqueue-ready 100)

(when (getenv "EMACS_PERF_PROFILE")
  (advice-add 'load :around #'perf-time-load)
  (advice-add 'require :around #'perf-time-load)
  (add-hook 'after-init-hook
            (lambda ()
              (setq emacs-startup-hook
                    (mapcar
                     (lambda (function)
                       (lambda ()
                         (let ((start (float-time)))
                           (prog1 (funcall function)
                             (push (list (truncate-string-to-width
                                          (format "HOOK %S" function) 160)
                                         (- (float-time) start))
                                   perf-startup-loads)))))
                     emacs-startup-hook)))
            99))

(provide 'perf-startup)
