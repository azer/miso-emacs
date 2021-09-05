(load-local-file "canggu-theme/canggu-theme")
(add-hook 'after-init-hook (lambda () (enable-theme 'canggu)))

(hl-line-mode)

(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled

