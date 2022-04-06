(add-hook 'web-mode-hook 'prettier-mode)
(add-hook 'typescript-mode-hook 'prettier-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))

(add-hook 'web-mode-hook
	  (lambda ()
	    (setq web-mode-markup-indent-offset 2)
            (setq web-mode-code-indent-offset 2)
            (when (string-equal "tsx" (file-name-extension buffer-file-name)) (typescript-mode))))

(add-hook 'before-save-hook 'lsp-organize-imports)


;; (defun setup-tide-mode ()
;;   (interactive)
;;   (tide-setup)
;;   (flycheck-mode +1)
;;   (setq flycheck-check-syntax-automatically '(save mode-enabled))
;;   (eldoc-mode +1)
;;   (tide-hl-identifier-mode +1)
;;   (company-mode +1))
;; ;; aligns annotation to the right hand side
;; (setq company-tooltip-align-annotations t)
;; ;; formats the buffer before saving
;;
;; (add-hook 'typescript-mode-hook #'setup-tide-mode)
;; (require 'web-mode)
;; (flycheck-add-mode 'typescript-tslint 'web-mode)
