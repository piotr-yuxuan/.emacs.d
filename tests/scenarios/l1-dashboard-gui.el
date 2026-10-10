;;; l1-dashboard-gui.el --- A new client frame shows the dashboard -*- lexical-binding: t -*-
;; scenario-display: graphic

;;; Commentary:
;; The smallest graphical scenario, which keeps the GUI tier and its
;; EMACS_TEST_GUI=0 switch verifiable: the client frame the runner
;; created shows the dashboard (`initial-buffer-choice'), no *Warnings*
;; buffer was created, and g refreshes the dashboard in place.
;;
;; Startup still logs warnings to *Messages* (the obsolete `when-let'
;; and `if-let', a nil :background in blamer-face); plan 085 hands them
;; to the configuration plans, and this file does not check them.
;;
;; Each row starts from the buffer the frame shows, so the order of
;; the scenarios matters; the refresh keeps the dashboard shown.

;;; Code:

(require 'scenario)

(scenario
 :describe "BL1-DASHBOARD-GUI: a new client frame shows the dashboard, no *Warnings* buffer"
 :given ((:name "client frame" :existing-buffer "\\`\\*dashboard\\*\\'"))
 :when ((:open))
 :then ((:mode dashboard-mode)
        (:buffer-matches "Welcome to Emacs")
        (:buffer-absent "\\`\\*Warnings\\*\\'")))

(scenario
 :describe "BL1-DASHBOARD-REFRESH: g keeps the empty startup screen usable"
 :given ((:name "dashboard" :existing-buffer "\\`\\*dashboard\\*\\'"))
 :when ((:keys "g"))
 :then ((:mode dashboard-mode)
        (:buffer-name "\\`\\*dashboard\\*\\'")
        (:buffer-matches "Welcome to Emacs")))

(provide 'l1-dashboard-gui)

;;; l1-dashboard-gui.el ends here
