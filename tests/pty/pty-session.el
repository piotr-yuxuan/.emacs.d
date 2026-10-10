;;; pty-session.el --- The shell of the PTY scenario sessions -*- lexical-binding: t -*-

;;; Commentary:
;; Required by each PTY scenario file (tests/scenarios/l3-pty-*.el) of
;; plan 091, before its scenarios run.  Every terminal of the session
;; then starts zsh with the fixture startup files of
;; tests/fixtures/pty-zdotdir/, never the user's own, which point
;; EDITOR at the user's own Emacs.  The session inherits ZDOTDIR from
;; the user's environment, so it is set here, not left unset.
;;
;; The fixture reads two more variables:
;; - PTY_FIXTURE_SERVER, the socket of this session's private server,
;;   for EDITOR and VISUAL;
;; - PTY_LATENCY_FILE, where `pty-throughput' writes its round trips,
;;   in the session's temporary directory, which the runner removes.

;;; Code:

(require 'server)

(defconst my/pty-zdotdir
  (expand-file-name "../fixtures/pty-zdotdir/"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "The fixture ZDOTDIR of the PTY scenarios.")

(setenv "ZDOTDIR" my/pty-zdotdir)
(setenv "PTY_FIXTURE_SERVER" (expand-file-name server-name server-socket-dir))
(setenv "PTY_LATENCY_FILE" (expand-file-name "pty-latency" temporary-file-directory))

;; The fixture startup files are zsh's.  gterm runs `gterm-shell',
;; zsh; mistty runs `explicit-shell-file-name', and eat `eat-shell',
;; which defaults to it when eat loads.  Without them, both would run
;; the user's login shell, whatever it is.
(defvar eat-shell)
(setq explicit-shell-file-name (executable-find "zsh")
      eat-shell explicit-shell-file-name)

;; The first use of a terminal package in a session compiles it to
;; native code in the background, and the compiler logs warnings, often
;; while a spec runs.  Buttercup prints a spec's warnings with
;; `send-string-to-terminal', which signals an error on a graphical
;; terminal, and that error ends the whole session.  Native-compiler
;; warnings say nothing about the terminals, so they are not logged.
(add-to-list 'warning-suppress-log-types '(native-compiler))

(provide 'pty-session)

;;; pty-session.el ends here
