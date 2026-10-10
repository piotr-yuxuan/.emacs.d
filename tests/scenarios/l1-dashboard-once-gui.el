;;; l1-dashboard-once-gui.el --- The dashboard is built for the client frame -*- lexical-binding: t -*-
;; scenario-display: graphic

;;; Commentary:
;; 084 CF-18: `initial-buffer-choice' builds the dashboard for each
;; frame that shows it.  A daemon also calls it at startup, for its
;; initial frame, which no one sees, and dashboard's own startup hooks
;; built it once more and showed it there.  A daemon with one client
;; frame must build the dashboard once, for that frame.  Each extra
;; build showed the dashboard in the initial frame, so that frame
;; showing it is how an extra build shows.

;;; Code:

(require 'scenario)

(scenario
 :describe "CF-18: the dashboard is shown in the client frame, not built for the initial one"
 :given ((:name "client frame" :existing-buffer "\\`\\*dashboard\\*\\'"))
 :when ((:open))
 :then ((:mode dashboard-mode)
        (:initial-frame-lacks "\\`\\*dashboard\\*\\'")))

(provide 'l1-dashboard-once-gui)

;;; l1-dashboard-once-gui.el ends here
