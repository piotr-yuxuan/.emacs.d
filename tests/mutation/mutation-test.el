;;; mutation-test.el --- Run the Python self-test of tests/mutate -*- lexical-binding: t -*-

;;; Commentary:
;; tests/mutate checks that the suite catches plausible regressions of
;; init.el; no tests/run profile runs it, as a whole check takes long.
;; Its self-test, test_mutation.py, checks the tool, and that every
;; mutant still fits init.el and names a test that exists.  This logic
;; test runs the self-test, so that `tests/run fast' fails as soon as a
;; change of init.el, of a journey or of the runner breaks a mutant,
;; rather than the next mutation check.  tests/mutate leaves this
;; directory out of its copies: there, init.el is mutated on purpose.

;;; Code:

(require 'buttercup)

(defconst my/mutation-tests-directory
  (expand-file-name "../" (file-name-directory (or load-file-name buffer-file-name)))
  "The tests/ directory, from which the tool's package is importable.")

(describe "The mutation check"
  (it "passes its self-test"
    (with-temp-buffer
      (let* ((default-directory my/mutation-tests-directory)
             (status (call-process (or (executable-find "python3") "python3") nil t nil
                                   "-m" "unittest" "-q" "mutation.test_mutation")))
        (unless (eql status 0)
          (buttercup-fail "test_mutation.py exited with %S:\n%s"
                          status (buffer-string)))))))

;;; mutation-test.el ends here
