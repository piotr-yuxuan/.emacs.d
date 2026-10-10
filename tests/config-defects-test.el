;;; config-defects-test.el --- Logic tests of plan 088 -*- lexical-binding: t -*-

;;; Commentary:
;; Logic tests of configuration code that plan 088 fixed, run in the
;; batch session of `tests/run' with the configuration loaded.  Stubs
;; stand only at system boundaries.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

(defvar my/straight-safe-update-boundaries)
(defvar my/vellum-directory)
(defvar my/nixd-flake)
(defvar my/nixd-flake-host)
(defvar lsp-nix-nixd-nixos-options-expr)
(defvar lsp-nix-nixd-home-manager-options-expr)
(defvar config-defects-test-vellum-file)
(defvar lsp-java-server-install-dir)
(defvar lsp-java-jdt-ls-prefer-native-command)
(defvar config-defects-test-mode-hook nil
  "The hook of a mode no grammar remaps, for the remap fallback.")
(declare-function my/straight-safe-update "init")
(declare-function my/treesit-remap-when-ready "init")
(declare-function lsp-java--ls-command "lsp-java")
(declare-function lsp-java--locate-server-jar "lsp-java")
(declare-function no-littering-expand-var-file-name "no-littering")

(defconst config-defects-test--root
  (file-name-as-directory
   (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name))))
  "The checkout under test, which holds tests/run.")

;; The real command must never run from the suite: it would pull every
;; package of the live straight.el tree.  Each boundary is stubbed but
;; :test, whose adapter runs against a stub runner.
(defvar config-defects-test--calls nil
  "The boundaries the last stubbed update called, newest first.")

(defun config-defects-test--update (unpulled tested &optional root rebuilt)
  "Run `my/straight-safe-update' with stubbed boundaries; return their calls.
The pull fails for the recipes UNPULLED, or signals it as (SYMBOL .
DATA).  TESTED is the exit status of the test run, or the error its
start signals.  ROOT is `user-emacs-directory', by default the checkout
under test.  REBUILT, when non-nil, is the signal of the first rebuild.
The calls are the boundary keys, in order, once the update has ended;
they are also in `config-defects-test--calls', newest first."
  (setq config-defects-test--calls nil)
  (let* ((my/straight-safe-update-boundaries
          (list :pull (lambda ()
                        (push :pull config-defects-test--calls)
                        (if (and unpulled (symbolp (car unpulled)))
                            (signal (car unpulled) (cdr unpulled))
                          unpulled))
                :rebuild (lambda ()
                           (push :rebuild config-defects-test--calls)
                           (when (and rebuilt (equal config-defects-test--calls '(:rebuild :pull)))
                             (signal (car rebuilt) (cdr rebuilt))))
                :test (lambda (_runner done)
                        (push :test config-defects-test--calls)
                        (if (integerp tested)
                            (funcall done tested)
                          (signal (car tested) (cdr tested))))
                :freeze (lambda () (push :freeze config-defects-test--calls))
                :thaw (lambda () (push :thaw config-defects-test--calls) nil)))
         (user-emacs-directory (or root config-defects-test--root))
         (inhibit-message t))
    (my/straight-safe-update)
    (reverse config-defects-test--calls)))

(describe "CF-11: the safe package update"

  (it "freezes the lockfile only when the tests pass"
    (expect (config-defects-test--update nil 0)
            :to-equal '(:pull :rebuild :test :freeze)))

  (it "thaws and rebuilds when the tests fail"
    (expect (config-defects-test--update nil 1)
            :to-equal '(:pull :rebuild :test :thaw :rebuild))
    ;; A test run killed by a signal reports the signal number.
    (expect (config-defects-test--update nil 15)
            :to-equal '(:pull :rebuild :test :thaw :rebuild)))

  (it "thaws and rebuilds, without testing, when a pull fails"
    ;; straight-pull-all returns the recipes it could not pull; the
    ;; others have merged their new commits.
    (expect (config-defects-test--update '((:package "magit")) 0)
            :to-equal '(:pull :thaw :rebuild)))

  (it "thaws and rebuilds when the pull signals or is quit"
    (expect (config-defects-test--update '(error "Merge failed") 0)
            :to-equal '(:pull :thaw :rebuild))
    (expect (config-defects-test--update '(quit) 0)
            :to-equal '(:pull :thaw :rebuild)))

  (it "thaws and rebuilds when the rebuild after the pull signals or is quit"
    (expect (config-defects-test--update nil 0 nil '(error "Could not find package foo"))
            :to-equal '(:pull :rebuild :thaw :rebuild))
    (expect (config-defects-test--update nil 0 nil '(quit))
            :to-equal '(:pull :rebuild :thaw :rebuild)))

  (it "refuses to start while the tests of a previous update run"
    (let ((previous (make-process :name "package-update-tests"
                                  :command '("sleep" "30")
                                  :noquery t)))
      (unwind-protect
          (expect (config-defects-test--update nil 0) :to-throw 'user-error)
        (delete-process previous))
      (expect config-defects-test--calls :to-equal nil)))

  (it "thaws and rebuilds when the tests cannot start"
    (expect (config-defects-test--update nil '(file-missing "Searching for program"))
            :to-equal '(:pull :rebuild :test :thaw :rebuild)))

  (it "changes nothing when there is no test runner to run"
    (let ((root (make-temp-file "config-defects-update-" t)))
      (unwind-protect
          (expect (config-defects-test--update nil 0 root) :to-throw 'user-error)
        (delete-directory root t))
      (expect config-defects-test--calls :to-equal nil)))

  (it "reports the exit status of the runner run with fast"
    ;; The :test adapter starts a real process: a stub runner exiting
    ;; with STATUS when its argument is fast, and 9 otherwise.
    (let ((root (make-temp-file "config-defects-runner-" t)))
      (unwind-protect
          (dolist (status '(0 3))
            (let ((runner (expand-file-name (format "run-%d" status) root))
                  (reported nil))
              (with-temp-file runner
                (insert (format "#!/bin/sh\n[ \"$1\" = fast ] || exit 9\nexit %d\n" status)))
              (set-file-modes runner #o700)
              (funcall (plist-get my/straight-safe-update-boundaries :test)
                       runner (lambda (exit) (setq reported exit)))
              (with-timeout (10 (error "The runner did not end"))
                (while (not reported)
                  (accept-process-output nil 0.05)))
              (expect reported :to-equal status)))
        (when-let* ((buffer (get-buffer "*Package update tests*")))
          (let ((kill-buffer-query-functions nil))
            (kill-buffer buffer)))
        (delete-directory root t)))))

(describe "PT-03: the vellum exporter comes from the directory local.el names"
  (it "loads vellum from that directory when ox-latex first loads"
    ;; As the first LaTeX export of a session does.  local.el runs at
    ;; the end of init, long before; here the let stands for it.
    (expect (featurep 'ox-latex) :to-be nil)
    (expect (featurep 'vellum) :to-be nil)
    (let ((my/vellum-directory
           (expand-file-name "tests/fixtures/vellum/" config-defects-test--root)))
      (require 'ox-latex)
      (expect (featurep 'vellum) :to-be-truthy)
      (expect (and (boundp 'config-defects-test-vellum-file)
                   (file-name-directory config-defects-test-vellum-file))
              :to-equal my/vellum-directory))))

(describe "CF-04: nixd completes the options of the flake local.el names"
  (it "builds the NixOS and Home Manager option sets once lsp-nix loads"
    ;; lsp-nix loads when the first Nix buffer starts a server, long
    ;; after local.el; the fixture local.el sets the flake, as a user's.
    (expect (featurep 'lsp-nix) :to-be nil)
    (let ((my/nixd-flake nil)
          (my/nixd-flake-host nil))
      (load (expand-file-name "tests/fixtures/local-nixd/local.el"
                              config-defects-test--root)
            nil t)
      (require 'lsp-nix))
    (expect lsp-nix-nixd-nixos-options-expr
            :to-equal "(builtins.getFlake \"/flakes/fleet/\").nixosConfigurations.fixture-host.options")
    (expect lsp-nix-nixd-home-manager-options-expr
            :to-equal (format "(builtins.getFlake \"/flakes/fleet/\").nixosConfigurations.fixture-host.config.home-manager.users.%s.options"
                              (user-login-name)))))

(describe "CF-15: lsp-java runs the jdtls launcher on PATH"
  (it "sees the installation's server, in a directory lsp-java may delete"
    ;; Each layout is (LAUNCHER PLUGINS), relative to an installation
    ;; whose bin/ is on PATH; bin/jdtls links to LAUNCHER when they
    ;; differ.  lsp-java deletes `lsp-java-server-install-dir' when it
    ;; installs a server, which must leave the installation whole.
    (let ((root (make-temp-file "config-defects-jdtls-" t)))
      (unwind-protect
          (cl-loop
           for (launcher plugins) in '(("bin/jdtls" "share/java/jdtls/plugins/")
                                       ("libexec/bin/jdtls" "libexec/plugins/")
                                       ("bin/jdtls" "libexec/plugins/"))
           for index from 0
           do (let* ((home (expand-file-name (format "layout-%d/" index) root))
                     (jdtls (expand-file-name "bin/jdtls" home))
                     (jar (expand-file-name "org.eclipse.equinox.launcher_1.0.0.jar"
                                            (expand-file-name plugins home))))
                (dolist (file (list jdtls (expand-file-name launcher home) jar))
                  (make-directory (file-name-directory file) t))
                (write-region "#!/bin/sh\nexit 1\n" nil (expand-file-name launcher home))
                (set-file-modes (expand-file-name launcher home) #o700)
                (unless (file-exists-p jdtls)
                  (make-symbolic-link (expand-file-name launcher home) jdtls))
                (write-region "" nil jar)
                (let ((exec-path (list (file-name-directory jdtls) exec-directory)))
                  ;; As the first Java buffer of a session loads it.
                  (load "lsp-java" nil t))
                (expect lsp-java-jdt-ls-prefer-native-command :to-be t)
                (expect (car (lsp-java--ls-command)) :to-equal jdtls)
                (expect (file-truename (lsp-java--locate-server-jar))
                        :to-equal (file-truename jar))
                (delete-directory lsp-java-server-install-dir t)
                (expect (file-exists-p jar) :to-be t)))
        (delete-directory root t))))

  (it "keeps running a server that lsp-mode installed in that directory"
    ;; `lsp-update-servers' replaces the plugins/ link with the
    ;; plugins/ directory of the server it installs.
    (let* ((root (make-temp-file "config-defects-jdtls-" t))
           (jdtls (expand-file-name "bin/jdtls" root))
           (install (no-littering-expand-var-file-name "lsp-java/jdtls-on-path/"))
           (jar (expand-file-name "plugins/org.eclipse.equinox.launcher_1.0.0.jar" install))
           (lsp-java-jdt-ls-prefer-native-command nil)
           (lsp-java-server-install-dir (expand-file-name "default/" root)))
      (unwind-protect
          (progn
            (make-directory (expand-file-name "share/java/jdtls/plugins/" root) t)
            (make-directory (file-name-directory jdtls) t)
            (write-region "#!/bin/sh\nexit 1\n" nil jdtls)
            (set-file-modes jdtls #o700)
            (make-directory (file-name-directory jar) t)
            (write-region "" nil jar)
            (let ((exec-path (list (file-name-directory jdtls) exec-directory)))
              (load "lsp-java" nil t))
            (expect lsp-java-jdt-ls-prefer-native-command :to-be nil)
            (expect (file-truename (lsp-java--locate-server-jar))
                    :to-equal (file-truename jar)))
        (delete-directory install t)
        (delete-directory root t)))))

(describe "DD-10: java-mode and dockerfile-mode start LSP when no grammar remaps them"
  (it "adds the fallback to the hook of the mode when its grammar is missing"
    (let ((config-defects-test-mode-hook nil)
          (major-mode-remap-alist nil))
      ;; No grammar exists for this language.
      (my/treesit-remap-when-ready 'config-defects-test-language
                                   'config-defects-test-mode 'text-mode #'lsp-deferred)
      (expect config-defects-test-mode-hook :to-equal '(lsp-deferred))
      (expect major-mode-remap-alist :to-equal nil)))

  (it "starts LSP from java-mode and dockerfile-mode exactly when they are not remapped"
    (expect (and (memq #'lsp-deferred (bound-and-true-p java-mode-hook)) t)
            :to-be (not (alist-get 'java-mode major-mode-remap-alist)))
    (expect (and (memq #'lsp-deferred (bound-and-true-p dockerfile-mode-hook)) t)
            :to-be (not (alist-get 'dockerfile-mode major-mode-remap-alist)))))

(describe "DD-08: the packages Emacs ships are not shadowed by straight.el builds"
  ;; Which copy of a library runs shows in no behaviour: the configured
  ;; behaviour is the same with both.  So this checks where each was
  ;; loaded from, for the libraries Emacs ships since 29.1 and 30.1.
  (it "loads which-key, editorconfig and use-package from Emacs when it ships them"
    (expect (seq-keep
             (pcase-lambda (`(,library . ,function))
               (let ((loaded (symbol-file function 'defun)))
                 (and (locate-file library (list lisp-directory) '(".elc" ".el" ".el.gz"))
                      (not (and loaded (file-in-directory-p loaded lisp-directory)))
                      (list library loaded))))
             '(("which-key" . which-key-mode)
               ("editorconfig" . editorconfig-mode)
               ("use-package/use-package-core" . use-package)))
            :to-equal nil)))

;;; config-defects-test.el ends here
