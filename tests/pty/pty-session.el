;;; pty-session.el --- The shell of the PTY scenario sessions -*- lexical-binding: t -*-

;;; Commentary:
;; Required by each PTY scenario file (tests/scenarios/l3-pty-*.el) of
;; plans 091 and 110, before its scenarios run.  The terminal of the
;; session then starts zsh with the fixture startup files of
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

;; The fixture startup files are zsh's: without `ghostel-shell',
;; ghostel would run the user's login shell, whatever it is.  The
;; module is never downloaded here: the session reads the live package
;; tree, which it must not write.  The row's cleanup lets a buffer be
;; killed without asking, but only the buffer then current: a cell
;; that fails with a command still running, emacsclient waiting in
;; another window for instance, would leave ghostel's own query to
;; wait for an answer until the session's deadline.
(defvar ghostel-shell)
(defvar ghostel-module-auto-install)
(defvar ghostel-query-before-killing)
(setq ghostel-shell (executable-find "zsh")
      ghostel-module-auto-install nil
      ghostel-query-before-killing nil)

(provide 'pty-session)

;;; pty-session.el ends here
