;;; l1-gterm-no-module.el --- gterm refuses to start without its module -*- lexical-binding: t -*-

;;; Commentary:
;; Plan 091, step 1: starting gterm never builds its Zig module.
;; Without the module, M-x gterm says which command builds it and
;; leaves gterm.el unloaded, since loading gterm.el builds the module
;; or asks to.  The session's gterm checkout is an empty temporary
;; directory, as on a machine where the module was never built: a
;; configuration that builds on load fails here on the missing
;; build.zig, before any clone or zig run.  gterm is never loaded in
;; this file's session, so it holds on a machine with the module too.

;;; Code:

(require 'scenario)

(setq gterm-source-dir (file-name-as-directory (make-temp-file "gterm-no-module-" t))
      gterm-module-path (expand-file-name "zig-out/lib/libgterm-module.so"
                                          gterm-source-dir))

(scenario
 :describe "gterm: without its module, M-x gterm names the command that builds it"
 :given ((:name "no module" :project "pty-project" :entry "start"))
 :when ((:keys "M-x") (:type "gterm") (:keys "<return>"))
 :then ((:user-error "\\`gterm: no module at .*; build it with M-x my/gterm-compile-module\\'")
        (:variable-excludes features gterm)
        (:buffer-absent "\\`\\*gterm")))

(unless my/macos-p
  (scenario
   :describe "gterm: the macOS SDK shim of its build is absent elsewhere"
   :given ((:name "no module" :project "pty-project" :entry "start"))
   :when ((:keys "M-x") (:type "gterm") (:keys "<return>"))
   :then ((:user-error "\\`gterm: no module at ")
          (:function-unadvised gterm-module-compile))))

(provide 'l1-gterm-no-module)

;;; l1-gterm-no-module.el ends here
