;;; package -- Summary  -*- lexical-binding: t; -*-
;;; Commentary:
;;;   Emacs configuration generated from =README.org=.
;;;
;;;   You can modify this file as you wish so, but keep in mind in the
;;;   long term it's better to keep a clean generation from the
;;;   literate documentation file.

;;; Code:

(setq package-enable-at-startup nil)

(defvar my/gc-cons-threshold (* 512 1024 1024) ; in bytes
  "My default desired value of `gc-cons-threshold'
during normal emacs operations.")

;; Automatic GC requires BOTH the byte threshold and the heap percentage
;; to be exceeded. A high startup threshold postpones automatic collection;
;; it cannot prevent a package from calling `garbage-collect' explicitly.
;; Restore a bounded runtime threshold after startup.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 1.0)

(defun my/restore-gc-threshold ()
  "Restore the runtime garbage collection thresholds after startup."
  (setq gc-cons-threshold my/gc-cons-threshold
        gc-cons-percentage 0.1))

(add-hook 'emacs-startup-hook #'my/restore-gc-threshold)

;; Don't check outdated compiled lisp files.
(setq load-prefer-newer noninteractive)

;; Asynchronous package compilation
(setq native-comp-jit-compilation t)

;; Suppress the *Compile-Log* buffer that byte-compile would otherwise open.
(setq byte-compile-verbose nil)

(defvar my/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(defun my/restore-file-name-handlers ()
  "Restore the file name handlers that startup ran without."
  (setq file-name-handler-alist my/file-name-handler-alist))

(add-hook 'emacs-startup-hook #'my/restore-file-name-handlers)

(setq site-run-file nil)

(setq vc-handled-backends '(Git))

(setq inhibit-compacting-font-caches t)

(setq inhibit-startup-screen nil
      ;; Open *scratch* in fundamental-mode with no welcome message —
      ;; the fastest possible startup buffer.  (lisp-interaction-mode,
      ;; the default, loads a minor amount of extra code at frame open.)
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

(setq read-process-output-max (* 4 1024 1024)
      process-adaptive-read-buffering nil)

(setq frame-inhibit-implied-resize t)
(setq default-frame-alist '((width . 95) (height . 45)))

;; Default all buffers to left-to-right; restore per-buffer when editing Arabic/Hebrew.
(setq-default bidi-paragraph-direction 'left-to-right)
;; Disable the expensive bidi parenthetical algorithm.
(setq bidi-inhibit-bpa t)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(provide 'early-init)
;;; early-init.el ends here
