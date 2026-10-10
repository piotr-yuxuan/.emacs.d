;;; file-manager-test.el --- Canary of the dirvish internals; the TAB key -*- lexical-binding: t -*-

;;; Commentary:
;; Plan 089, L2 configuration sanity.
;;
;; The File manager section of README.org relies on private dirvish
;; definitions (double-dash names): it overrides
;; `dirvish-collapse--cache' and calls subtree internals.  Upstream may
;; change them in any release.  The scenarios of l1-dirvish-tree.el
;; catch a change that breaks what the user sees, but not one that
;; makes the override drift silently from upstream.  No behaviour shows
;; that a definition changed, so this canary compares the source of
;; each one with the source README.org was written against.  When it
;; fails, re-read the changed definition, adapt README.org, and update
;; its hash here.
;;
;; Scenarios press keys by their graphical names and reject TAB (see
;; keystrokes.el), so no scenario can press the TAB of a terminal; the
;; second suite checks that it runs the same command as <tab>.

;;; Code:

(require 'buttercup)
(require 'dirvish)

(defconst file-manager-test--init
  (expand-file-name "../init.el" (file-name-directory (or load-file-name buffer-file-name)))
  "The init.el under test.")

(defconst file-manager-test--internals
  '(("dirvish-collapse" dirvish-collapse--cache
     "a66beb9ffc268079390578b7154b59653d2adb7be46907bd7825ab4bd2d025ec")
    ("dirvish-collapse" collapse
     "ac3295924d363bbf3fe56ad73712cdfa7f30234e9cd8cd00bdaee1043007dbe0")
    ("dirvish-subtree" dirvish-subtree--expanded-p
     "62dd05964a4ef9bdbe46f5fc3250662b7f6282546b02358f24313d313b8931f9")
    ("dirvish-subtree" dirvish-subtree--parent
     "7b0aaedf90e8482fa6a55447cfb13afdff14a50e112e979e1fba1289435aaae1")
    ("dirvish-subtree" dirvish-subtree--overlays
     "77a36b2d6329d0a93d41252f0d02cc20fbab38d50ba075bee7cb80d4730e2833"))
  "The upstream definitions README.org relies on: (LIBRARY NAME SHA256).
SHA256 hashes the source text of the top-level form of LIBRARY that
defines NAME.  `collapse' is the attribute that calls
`dirvish-collapse--cache' and reads what it returns.")

(defun file-manager-test--forms (file)
  "Return the top-level forms of the Lisp source FILE, as (FORM . TEXT).
TEXT is the source text of FORM."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((forms nil))
      (condition-case nil
          (while t
            (forward-comment (buffer-size))
            (let* ((start (point))
                   (form (read (current-buffer))))
              (push (cons form (buffer-substring-no-properties start (point))) forms)))
        (end-of-file (nreverse forms))))))

(describe "The private dirvish definitions README.org relies on"

  (it "are the upstream versions README.org was written against"
    (let ((changed nil))
      (pcase-dolist (`(,library ,name ,hash) file-manager-test--internals)
        (let* ((source (locate-library (concat library ".el") t))
               (form (seq-find (lambda (form) (eq (car-safe (cdr-safe (car form))) name))
                               (file-manager-test--forms source)))
               (actual (and form (secure-hash 'sha256 (cdr form)))))
          (unless (equal actual hash)
            (push (format "%s in %s: %s" name source (or actual "gone")) changed))))
      (expect (nreverse changed) :to-equal nil)))

  (it "include every private dirvish name init.el uses"
    (let ((used (seq-uniq
                 (seq-filter (lambda (symbol)
                               (and (symbolp symbol)
                                    (string-match-p "\\`dirvish.*--" (symbol-name symbol))))
                             (flatten-tree (mapcar #'car (file-manager-test--forms
                                                          file-manager-test--init)))))))
      (expect (seq-difference used (mapcar #'cadr file-manager-test--internals))
              :to-equal nil))))

(describe "The private Emacs definition README.org relies on"
  ;; RET on a chain calls it; the hash check above reads dirvish
  ;; sources only, and the Emacs sources may be compressed.
  (it "dired--find-possibly-alternative-file still takes one file name"
    (expect (func-arity #'dired--find-possibly-alternative-file) :to-equal '(1 . 1))))

(describe "In a dirvish buffer"
  (it "TAB, the Tab key of a terminal, runs the command of <tab>"
    (expect (keymap-lookup dirvish-mode-map "TAB")
            :to-be (keymap-lookup dirvish-mode-map "<tab>"))))

(provide 'file-manager-test)

;;; file-manager-test.el ends here
