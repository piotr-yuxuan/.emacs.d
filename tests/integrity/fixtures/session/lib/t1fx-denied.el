;;; t1fx-denied.el --- A fixture library no check may load -*- lexical-binding: t -*-

;;; Commentary:
;; The self-test of the variable library loader denies this library,
;; as the force-load denies gterm: `t1fx-denied-loaded' is bound only
;; if something loaded it anyway.

;;; Code:

(defvar t1fx-denied-loaded t
  "Bound once this library is loaded.")

(provide 't1fx-denied)

;;; t1fx-denied.el ends here
