;;; run.el --- Run the static tier: emacs --batch -Q -l run.el [FILTER] -*- lexical-binding: t -*-

;;; Commentary:
;; The entry point of `tests/run static'.  It loads buttercup from the
;; live straight.el build, then the files of this directory that hold
;; specs, `t0.el' and the `*-test.el' logic tests of the checks, keeping
;; those whose path under tests/ contains FILTER.  It runs their specs
;; and exits with 0 when every spec passes, 1 otherwise.  Nothing of
;; the configuration is loaded.

;;; Code:

(let* ((here (file-name-directory load-file-name))
       (tests (expand-file-name "../../" here))
       (filter (or (pop command-line-args-left) ""))
       (files (seq-filter (lambda (file) (string-search filter (file-relative-name file tests)))
                          (directory-files here t "\\(?:-test\\|\\`t0\\)\\.el\\'"))))
  (add-to-list 'load-path (expand-file-name ".." here))
  (add-to-list 'load-path here)
  (require 't0-checks)
  (add-to-list 'load-path (expand-file-name "straight/build/buttercup" my/integrity-live-directory))
  (unless (require 'buttercup nil t)
    (message "No buttercup build under %s; set EMACS_LIVE_DIR." my/integrity-live-directory)
    (kill-emacs 1))
  (unless files
    (message "No static test matches %S." filter)
    (kill-emacs 1))
  (mapc #'load files)
  ;; As buttercup's own entry points do: otherwise each failing spec
  ;; prints Emacs's batch backtrace, whose closures hold the whole
  ;; configuration read as data (megabytes for one failure).
  (setq backtrace-on-error-noninteractive nil)
  (kill-emacs (if (eq (buttercup-run t) t) 0 1)))

;;; run.el ends here
