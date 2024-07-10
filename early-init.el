(setq package-enable-at-startup nil)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(menu-bar-mode . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(set-face-attribute 'default nil
                    :family (cond ((eq system-type 'darwin) "Iosevka Term")
				  ((eq system-type 'gnu/linux) "Iosevka Term")
				  (t "Fira Code"))
                    :weight 'regular
                    :height 180
		    :width 'normal)

(provide 'early-init)
;;; early-init.el ends here
