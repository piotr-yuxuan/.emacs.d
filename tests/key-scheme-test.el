;;; key-scheme-test.el --- The which-key labels of the prefixes -*- lexical-binding: t -*-

;;; Commentary:
;; Plan 093, 084 DD-10, L2 configuration sanity.
;;
;; which-key names a prefix's continuations in a popup that an idle
;; timer of the command loop opens, and only while no command runs.  A
;; scenario's keys run in a keyboard macro, where which-key opens no
;; popup, and they cannot end inside a prefix; so no scenario can show
;; the popup.  These specs ask which-key for each label as the popup
;; does, through `which-key--maybe-replace', in buffers of several
;; major modes: a label of a global prefix must not depend on the mode,
;; and a label of a Python prefix must appear in Python buffers only.

;;; Code:

(require 'buttercup)
(require 'which-key)

(defun key-scheme-test--labels (mode keys)
  "Return the labels which-key shows for the prefix KEYS in a MODE buffer.
A key without a label shows its binding, here \"prefix\"."
  (let ((major-mode mode))
    (mapcar (lambda (key) (cdr (which-key--maybe-replace (cons key "prefix"))))
            keys)))

(describe "DD-10: the which-key labels of the prefixes"

  (it "name the global prefixes in buffers of every mode"
    (dolist (mode '(fundamental-mode text-mode python-mode python-ts-mode))
      (expect (cons mode (key-scheme-test--labels
                          mode '("C-c s" "C-c z" "C-c g" "C-x t" "C-c d" "C-c k")))
              :to-equal (list mode "Search files" "Fold code" "Change hunks" "Tabs"
                              "Debug" "Run tests"))))

  (it "name the Python prefixes in Python buffers only"
    (dolist (mode '(python-mode python-ts-mode))
      (expect (cons mode (key-scheme-test--labels mode '("C-c C-t" "C-c TAB")))
              :to-equal (list mode "Insert Python templates" "Manage Python imports")))
    (expect (key-scheme-test--labels 'text-mode '("C-c C-t" "C-c TAB"))
            :to-equal '("prefix" "prefix"))))

;;; key-scheme-test.el ends here
