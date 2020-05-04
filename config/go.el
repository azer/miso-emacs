(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; (add-hook 'go-mode-hook (lambda ()
;;			    (interactive)
;;			    (local-set-key "\M-i " 'flycheck-list-errors)
;;			    ))

(require 'dap-go)
