;;; driver-test.el --- Run the Python self-test of the T1 driver -*- lexical-binding: t -*-

;;; Commentary:
;; tests/integrity/session/driver.py runs T1 through sessions of the
;; runner of plan 085, and no tests/run profile runs T1.  Its self-test,
;; test_driver.py, checks that the driver calls that runner as the
;; runner defines it.  This logic test runs the self-test, so that
;; `tests/run fast' fails as soon as the runner changes under the
;; driver, rather than the next manual T1 run.

;;; Code:

(require 'buttercup)

(defconst my/t1-driver-tests-directory
  (expand-file-name "../../" (file-name-directory (or load-file-name buffer-file-name)))
  "The tests/ directory, from which the driver's package is importable.")

(describe "The T1 driver"
  (it "calls the runner of plan 085 as the runner defines it"
    (with-temp-buffer
      (let* ((default-directory my/t1-driver-tests-directory)
             (status (call-process (or (executable-find "python3") "python3") nil t nil
                                   "-m" "unittest" "-q" "integrity.session.test_driver")))
        (unless (eql status 0)
          (buttercup-fail "test_driver.py exited with %S:\n%s" status (buffer-string)))))))

;;; driver-test.el ends here
