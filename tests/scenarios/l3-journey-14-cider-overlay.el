;;; l3-journey-14-cider-overlay.el --- Journey 14: CIDER shows an evaluation as an overlay -*- lexical-binding: t -*-
;; scenario-tier: l3

;;; Commentary:
;; Plan 094, journey 14 (the CIDER journey of plan 064).  In a Clojure
;; CLI project the user jacks in with C-c M-j, waits for the REPL,
;; which the configuration shows without selecting it, then evaluates
;; the form on the current line with C-e C-x C-e.  The result appears
;; as an overlay at the end of the line (`cider-eval-result-display',
;; `cider-eval-result-position').  Showing results in the echo area
;; only, the mutation, leaves the line as it is.
;;
;; Jack-in starts a JVM with nREPL, cider-nrepl and refactor-nrepl from
;; the local Maven repository; tests never download.  When the
;; cider-nrepl version this CIDER asks for is not cached, the newest
;; cached one is injected instead (CIDER then warns about the version
;; in the REPL); when any injected artifact is still missing, the file
;; refuses to load, naming it, rather than download it.  Without
;; clojure or java nothing is checked: the spec then skips, as
;; tests/expected-skips/094.el declares.  A JVM REPL
;; takes 20 to 40 s to start on a busy machine, and the keys cannot
;; wait for a condition, so the user waits 90 s before evaluating.
;;
;; Babashka would start in a second, but this CIDER sends
;; cider/init-debugger to every Clojure runtime: without cider-nrepl
;; the connection then stops before CIDER turns on in the source
;; buffers (an upstream problem; see the plan's open questions).

;;; Code:

(require 'scenario)
(require 'cider)
(require 'clj-refactor)

(when (and (executable-find "clojure") (executable-find "java"))
  (let* ((repository (expand-file-name "~/.m2/repository/"))
         (jar (lambda (path artifact version)
                (format "%s%s/%s/%s/%s-%s.jar" repository path artifact version artifact version)))
         (middleware (and (file-directory-p (concat repository "cider/cider-nrepl/"))
                          (seq-filter
                           (lambda (version)
                             (file-exists-p (funcall jar "cider" "cider-nrepl" version)))
                           (directory-files (concat repository "cider/cider-nrepl/")
                                            nil "\\`[0-9]")))))
    (unless (member cider-injected-middleware-version middleware)
      (setq cider-injected-middleware-version
            (or (car (last (sort middleware #'version<))) "none")))
    (dolist (file (list (funcall jar "nrepl" "nrepl" cider-injected-nrepl-version)
                        (funcall jar "cider" "cider-nrepl" cider-injected-middleware-version)
                        (funcall jar "refactor-nrepl" "refactor-nrepl"
                                 cljr-injected-middleware-version)))
      (unless (file-exists-p file)
        (error "Jack-in would download %s: jack in once by hand to cache it" file)))))

(scenario
 :describe "J-14: CIDER jacks in, C-x C-e shows the result as an overlay"
 :given ((:name "clojure" :project "journey-cider" :entry "src/journey/sums.clj"
          :point-on "(+ 1 2)" :system (:lsp-root ".")))
 :when ((:keys "C-c M-j") (:idle 60000) (:idle 30000) (:keys "C-e C-x C-e") (:idle 15000))
 :then ((:executables-found "clojure" "java")
        (:window-shows "\\`\\*cider-repl ")
        (:line-display-matches "^(\\+ 1 2)$" "(\\+ 1 2) => 3")))

(provide 'l3-journey-14-cider-overlay)

;;; l3-journey-14-cider-overlay.el ends here
