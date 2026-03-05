;;; package -- Summary  -*- lexical-binding: t; -*-
;;; Commentary:
;;;   Emacs configuration generated from =init.org=.
;;;
;;;   You can modify this file as you wish so, but keep in mind in the
;;;   long term it's better to keep a clean generation from the
;;;   literate documentation file.

;;; Code:

(setq package-enable-at-startup nil)

(defvar my/gc-cons-threshold (* 512 1024 1024) ; in bytes
  "My default desired value of `gc-cons-threshold'
during normal emacs operations.")

;; Suppress GC entirely during startup. `gc-cons-threshold' blocks the
;; byte-count trigger; `gc-cons-percentage' must also be raised because
;; Emacs has a second, independent trigger: GC fires when bytes consed
;; since last GC exceed gc-cons-percentage × current-heap-size.  At 0.8
;; this fires multiple times as the heap grows from ~50 MB to ~500 MB
;; while loading packages — even though the threshold is maxed out.
;; Setting it to 1.0 means GC only fires when allocations equal the
;; entire current heap, which never happens during normal loading.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 1.0)

(add-hook
 'emacs-startup-hook
 (lambda (&rest _)
   (setq gc-cons-threshold my/gc-cons-threshold
         gc-cons-percentage 0.1)))

;; Don't check outdated compiled lisp files.
(setq load-prefer-newer noninteractive)

;; Asynchronous package compilation
(setq native-comp-jit-compilation t)

;; Suppress the *Compile-Log* buffer that byte-compile would otherwise open.
(setq byte-compile-verbose nil)

(defvar my/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist my/file-name-handler-alist)))

(setq site-run-file nil)

(setq vc-handled-backends '(Git))

(setq inhibit-compacting-font-caches t)

(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message (user-login-name)
      ;; Open *scratch* in fundamental-mode with no welcome message —
      ;; the fastest possible startup buffer.  (lisp-interaction-mode,
      ;; the default, loads a minor amount of extra code at frame open.)
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

(when (boundp 'read-process-output-max)
  ;; 1MB in bytes, default 4096 bytes
  (setq read-process-output-max 1048576))

(setq frame-inhibit-implied-resize t)

;; Default all buffers to left-to-right; restore per-buffer when editing Arabic/Hebrew.
(setq-default bidi-paragraph-direction 'left-to-right)
;; Disable the expensive bidi parenthetical algorithm.
(setq bidi-inhibit-bpa t)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(menu-bar-mode . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq org-fold-core-style 'overlays)

(provide 'early-init)
;;; early-init.el ends here
