;;; t1fx-lazy.el --- A fixture library nothing loads -*- lexical-binding: t -*-

;;; Commentary:
;; Not loaded by live.el: the variable check must load it, by the
;; prefix of `t1fx-lazy-option', to find the option declared.

;;; Code:

(defcustom t1fx-lazy-option nil
  "An option of a library that loads late."
  :type 'boolean
  :group 'emacs)

(provide 't1fx-lazy)

;;; t1fx-lazy.el ends here
