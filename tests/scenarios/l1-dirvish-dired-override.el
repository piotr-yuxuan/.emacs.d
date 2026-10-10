;;; l1-dirvish-dired-override.el --- find-file on a directory routes through dirvish -*- lexical-binding: t -*-

;;; Commentary:
;; Plan 063 / dired-override slice.  The live config activates
;; `dirvish-override-dired-mode' eagerly on `emacs-startup-hook'; this
;; scenario opens a directory and asserts that dirvish manages the
;; resulting buffer, which plain dired does not satisfy (TS-19, plan
;; 089).

;;; Code:

(require 'scenario)

(scenario
 :describe "BL1-DIRVISH-OVERRIDE: find-file on a directory uses dirvish"
 :given ((:name "directory-as-find-file-target"
          :project "python-uv"
          :entry "src"))
 :when ((:open))
 :then ((:mode dired-mode)
        (:dirvish-session)))

(provide 'l1-dirvish-dired-override)

;;; l1-dirvish-dired-override.el ends here
