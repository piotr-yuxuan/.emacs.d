;;; vellum.el --- Fixture standing in for the vellum exporter -*- lexical-binding: t -*-

;; tests/config-defects-test.el names this directory as the machine's
;; vellum directory and checks that loading ox-latex loads this file.

(defvar config-defects-test-vellum-file load-file-name
  "The file this fixture was loaded from.")

(provide 'vellum)

;;; vellum.el ends here
