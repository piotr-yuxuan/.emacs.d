;;; outcomes/sockets-test.el --- Self-tests of the socket outcomes -*- lexical-binding: t -*-

;;; Commentary:
;; A listening local socket accepts a connection; a missing file, a
;; plain file and a variable that holds no directory each fail.

;;; Code:

(require 'buttercup)
(require 'scenario/outcome)
(require 'outcomes)

(defvar outcomes-sockets-test--directory nil
  "The directory of the sockets these tests open.")

(defun outcomes-sockets-test--verdict (clause)
  "Return the verdict of CLAUSE."
  (my/scenario-check-clause clause)
  (my/scenario-outcome-verdict clause nil))

(describe "Socket outcomes"
  (it ":socket-accepts holds for a listening socket only"
    (let* ((outcomes-sockets-test--directory (make-temp-file "sock-" t))
           (server (make-network-process
                    :name "outcomes-sockets-test" :family 'local :server t :noquery t
                    :service (expand-file-name "s" outcomes-sockets-test--directory))))
      (unwind-protect
          (progn
            (write-region "" nil (expand-file-name "plain" outcomes-sockets-test--directory))
            (expect (outcomes-sockets-test--verdict
                     '(:socket-accepts outcomes-sockets-test--directory "s"))
                    :to-be nil)
            (expect (outcomes-sockets-test--verdict
                     '(:socket-accepts outcomes-sockets-test--directory "missing"))
                    :to-match "does not exist")
            (expect (outcomes-sockets-test--verdict
                     '(:socket-accepts outcomes-sockets-test--directory "plain"))
                    :to-match "connecting to .* fails"))
        (delete-process server)
        (delete-directory outcomes-sockets-test--directory t))))

  (it ":socket-accepts fails when the variable holds no directory"
    (expect (outcomes-sockets-test--verdict
             '(:socket-accepts outcomes-sockets-test-unbound "s"))
            :to-match "holds no directory")))

;;; outcomes/sockets-test.el ends here
