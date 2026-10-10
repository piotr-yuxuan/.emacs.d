;;; scenario/display.el --- Text as the user sees it -*- lexical-binding: t -*-

;;; Commentary:
;; Outcomes and samples sometimes need the text as displayed rather
;; than the buffer text: an inlay hint is an overlay string, a fold
;; hides text, a completion UI draws its candidates as an overlay
;; after the minibuffer input.  `my/scenario-displayed-text' rebuilds
;; that text from the buffer and its properties, without redisplay, so
;; it reads the same in a graphical frame, a terminal frame or batch.

;;; Code:

(defun my/scenario-displayed-text (beg end)
  "Return the text from BEG to END of the current buffer as displayed.
The result is the buffer text without its invisible parts, where a
string `display' property replaces the text it covers, and where
overlay before-strings and after-strings appear at their positions:
at one position, the after-strings of overlays ending there come
first, then before-strings, then the after-strings of empty overlays,
then the text.  Other display specifications, such as images or
`space', leave the buffer text unchanged.  Text properties are
dropped."
  (let ((pieces nil)
        (pos beg))
    ;; Each piece is (KEY STRING); KEY orders pieces by position, then
    ;; by kind within one position.
    (dolist (overlay (overlays-in beg (min (1+ end) (point-max))))
      (push (list (1+ (* 4 (overlay-start overlay)))
                  (overlay-get overlay 'before-string))
            pieces)
      (push (list (+ (* 4 (overlay-end overlay))
                     (if (= (overlay-start overlay) (overlay-end overlay)) 2 0))
                  (overlay-get overlay 'after-string))
            pieces))
    (while (< pos end)
      (let ((next (min (next-single-char-property-change pos 'invisible nil end)
                       (next-single-char-property-change pos 'display nil end)
                       (next-overlay-change pos)))
            (display (get-char-property pos 'display)))
        (push (list (+ (* 4 pos) 3)
                    (cond ((invisible-p pos) nil)
                          ((stringp display) display)
                          (t (buffer-substring-no-properties pos next))))
              pieces)
        (setq pos next)))
    (mapconcat (lambda (piece) (substring-no-properties (cadr piece)))
               (sort (seq-filter (lambda (piece)
                                   (and (stringp (cadr piece))
                                        (<= (* 4 beg) (car piece) (+ (* 4 end) 3))))
                                 pieces)
                     (lambda (a b) (< (car a) (car b))))
               "")))

(provide 'scenario/display)

;;; scenario/display.el ends here
