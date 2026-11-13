;;; release-script-test.el --- Run the tests of the public release script -*- lexical-binding: t -*-

;;; Commentary:
;; The release script of the public repository, release.py under
;; publish/overlay/.github/scripts/, runs only in its CD.  Its tests
;; run on the public CI, after an export; this logic test runs them
;; here too, so that `tests/run fast' fails before a broken release
;; script or cd.yml is exported (plan 113).

;;; Code:

(require 'buttercup)

(defconst my/release-script-directory
  (expand-file-name "../publish/overlay/.github/scripts/"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "The directory of the release script and its tests.")

(describe "The release script of the public repository"
  (it "passes its tests"
    (with-temp-buffer
      (let* ((default-directory my/release-script-directory)
             (status (call-process (or (executable-find "python3") "python3") nil t nil
                                   "-m" "unittest" "-q" "release_test")))
        (unless (eql status 0)
          (buttercup-fail "release_test.py exited with %S:\n%s"
                          status (buffer-string)))))))

;;; release-script-test.el ends here
