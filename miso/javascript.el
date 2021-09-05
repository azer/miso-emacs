(add-hook 'js2-mode-hook 'prettier-js-mode)
(add-hook 'rjsx-mode 'prettier-js-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))

(custom-set-variables '(js2-strict-inconsistent-return-warning nil))
(custom-set-variables '(js2-strict-missing-semi-warning nil))

(setq js-indent-level 2)
(setq js2-indent-level 2)
(setq js2-basic-offset 2)

(setq web-mode-markup-indent-offset 2)
            (setq web-mode-code-indent-offset 2)

(add-hook 'web-mode-hook
           (lambda ()
	    (when (string-equal "jsx" (file-name-extension buffer-file-name))
               (rjsx-mode))
	    ))
