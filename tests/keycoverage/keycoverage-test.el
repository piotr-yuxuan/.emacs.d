;;; keycoverage-test.el --- Run the Python self-test of tests/coverage -*- lexical-binding: t -*-

;;; Commentary:
;; tests/coverage reports which key bindings the tests exercise; no
;; tests/run profile runs it.  Its self-test, test_keycoverage.py,
;; checks its rules, its reading of plan 086's binding table, and that
;; it calls the runner of plan 085 as the runner defines it.  This
;; logic test runs the self-test, so that `tests/run fast' fails as soon
;; as the runner or the binding table changes under the report.

;;; Code:

(require 'buttercup)

(defconst my/keycoverage-tests-directory
  (expand-file-name "../" (file-name-directory (or load-file-name buffer-file-name)))
  "The tests/ directory, from which the report's package is importable.")

(describe "The key-binding coverage report"
  (it "passes its self-test"
    (with-temp-buffer
      (let* ((default-directory my/keycoverage-tests-directory)
             (status (call-process (or (executable-find "python3") "python3") nil t nil
                                   "-m" "unittest" "-q" "keycoverage.test_keycoverage")))
        (unless (eql status 0)
          (buttercup-fail "test_keycoverage.py exited with %S:\n%s"
                          status (buffer-string)))))))

;;; keycoverage-test.el ends here
