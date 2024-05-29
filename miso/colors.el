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

;;(hl-line-mode)
;;(global-hl-line-mode)

(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled
