;;; l1-magit-delta-appearance.el --- Delta colours follow the appearance -*- lexical-binding: t -*-

;;; Commentary:
;; delta colours magit's diffs for a light or a dark background, as
;; its theme argument says (084 CF-17).  The argument must follow the
;; desktop appearance whether magit-delta loads before or after the
;; appearance changes.  The first scenario switches to light before
;; magit, and so magit-delta, first loads in the session; the second,
;; run after it, switches to dark once magit-delta is loaded.  The
;; scenarios share the session, so their order matters.

;;; Code:

(require 'scenario)

(scenario
 :describe "CF-17: magit-delta loaded in a light session colours for light"
 :given ((:name "light first" :file "notes.txt" :text "light-!-\n"
          :system (:appearance light)))
 :when ((:keys "C-x g"))
 :then ((:mode magit-status-mode)
        (:variable-contains magit-delta-delta-args "--light")
        (:variable-excludes magit-delta-delta-args "--dark")))

(scenario
 :describe "CF-17: an appearance change after magit-delta loads reaches delta"
 :given ((:name "dark later" :file "notes.txt" :text "dark-!-\n"
          :system (:appearance dark)))
 :when ((:keys "C-x g"))
 :then ((:mode magit-status-mode)
        (:variable-contains magit-delta-delta-args "--dark")
        (:variable-excludes magit-delta-delta-args "--light")))

(provide 'l1-magit-delta-appearance)

;;; l1-magit-delta-appearance.el ends here
