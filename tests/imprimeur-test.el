;;; imprimeur-test.el --- Logic tests of plan 112 -*- lexical-binding: t -*-

;;; Commentary:
;; The configuration installs imprimeur with every skin it ships.  A
;; skin that fails to load is hidden by its autoload's
;; `with-demoted-errors', so the registry is where a missing recipe or
;; a skin that does not build shows.

;;; Code:

(require 'buttercup)

(declare-function imprimeur-skin-names "imprimeur-skin")

(describe "imprimeur"
  (it "registers the nine skins of release 0.0.10 once it loads"
    (require 'imprimeur-skin)
    (expect (imprimeur-skin-names)
            :to-equal '("base" "briefing" "dubai" "ecrivain" "minitel"
                        "tianlu" "xix" "xvii" "xviii" "yaqut"))))

;;; imprimeur-test.el ends here
