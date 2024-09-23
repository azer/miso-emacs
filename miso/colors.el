(load-local-file "canggu-theme/canggu-theme")
(add-hook 'after-init-hook (lambda ()
			     (enable-theme 'canggu)
			     (set-face-attribute 'default nil
						 :family "Inconsolata"
						 :height 170
						 :weight 'normal
						 :width 'normal)

			     ))

(add-hook 'after-init-hook (doom-modeline-mode))

(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled

(custom-set-variables
 '(window-divider-default-bottom-width 1)
 '(window-divider-default-places t)
 '(window-divider-default-right-width 1)
 '(window-divider-mode t))
