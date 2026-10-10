;;; outcomes/config-defects.el --- Outcomes of plan 088 -*- lexical-binding: t -*-

;;; Commentary:
;; Outcomes the scenarios of plan 088 (configuration defects) need and
;; the core set lacks.  See `scenario/outcome.el' for the contract.

;;; Code:

(require 'seq)
(require 'scenario/outcome)

(my/scenario-define-outcome :longest-line-within (_context min max)
  "The longest line of the buffer is MIN to MAX columns wide."
  (let ((longest (seq-max (mapcar #'string-width
                                  (split-string (buffer-substring-no-properties
                                                 (point-min) (point-max))
                                                "\n")))))
    (unless (<= min longest max)
      (format "the longest line is %d columns wide, not %d to %d" longest min max))))

(my/scenario-define-outcome :mode-line-matches (_context regexp)
  "The mode line of the selected window shows text matching REGEXP.
Emacs formats no mode line in batch or in the initial frame of a
daemon, so a file using this outcome declares `scenario-display:
graphic'."
  (let ((case-fold-search nil)
        (shown (substring-no-properties
                (format-mode-line mode-line-format nil (selected-window)
                                  (current-buffer)))))
    (unless (string-match-p regexp shown)
      (format "the mode line is %S" shown))))

(my/scenario-define-outcome :initial-frame-lacks (_context regexp)
  "No window of the daemon's initial frame shows a buffer matching REGEXP.
That frame, `terminal-frame', is never shown to the user.  Fail when it
is not live, which would make the check vacuous."
  (if (not (frame-live-p terminal-frame))
      "the initial frame is not live"
    (when-let* ((shown (seq-filter (lambda (name) (string-match-p regexp name))
                                   (mapcar (lambda (window) (buffer-name (window-buffer window)))
                                           (window-list terminal-frame 'no-minibuffer)))))
      (format "the initial frame shows %S" shown))))

(provide 'outcomes/config-defects)

;;; outcomes/config-defects.el ends here
