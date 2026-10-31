;;; auto-dark-test.el --- Logic tests of plan 106 -*- lexical-binding: t -*-

;;; Commentary:
;; Run in the batch session of `tests/run' with the configuration
;; loaded.  auto-dark reads the appearance from the system: on Linux,
;; from the desktop portal on the session's D-Bus.  Where nothing tells
;; it, as on a CI runner or in a test session, it only logs an error at
;; each start; `my/auto-dark' then leaves it off.  The stubs stand at
;; the system boundary, D-Bus, and at the mode it would start.

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'auto-dark)

(declare-function my/auto-dark "init")

(defun auto-dark-test--warnings ()
  "Return the text of the *Warnings* buffer, or nil without one."
  (and (get-buffer "*Warnings*")
       (with-current-buffer "*Warnings*" (buffer-string))))

(describe "AD-01: auto-dark starts only where it can read the appearance"
  (it "stays off, without a warning, where nothing tells the appearance"
    (let ((auto-dark-detection-method nil)
          (system-type 'gnu/linux)
          (started nil)
          (before (auto-dark-test--warnings)))
      (cl-letf (((symbol-function 'dbus-list-activatable-names) #'ignore)
                ((symbol-function 'auto-dark-mode)
                 (lambda (&rest arguments) (setq started arguments))))
        (my/auto-dark))
      (expect started :to-be nil)
      (expect (auto-dark-test--warnings) :to-equal before)))

  (it "starts with the method it found where the desktop portal answers"
    (let ((auto-dark-detection-method nil)
          (system-type 'gnu/linux)
          (features (cons 'dbus features))
          (started nil))
      (cl-letf (((symbol-function 'dbus-list-activatable-names)
                 (lambda (&rest _) '("org.freedesktop.portal.Desktop")))
                ((symbol-function 'auto-dark-mode)
                 (lambda (&rest arguments) (setq started arguments))))
        (my/auto-dark)
        (expect started :to-equal '(t))
        (expect auto-dark-detection-method :to-be 'dbus)))))

(provide 'auto-dark-test)

;;; auto-dark-test.el ends here
