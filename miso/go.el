;; Basic Go mode configuration
(require 'dap-go)

;; LSP hooks and configuration for Go
(add-hook 'go-mode-hook #'lsp-deferred)
(add-hook 'go-ts-mode-hook #'lsp-deferred)

;; Configure gopls settings
(with-eval-after-load 'lsp-mode
  (setq lsp-go-analyses
        '((shadow . t)
          (unusedparams . t)
          (unusedwrite . t)
          (useany . t)
          (fillreturns . t)))

  (setq lsp-go-hover-kind "FullDocumentation"
        lsp-go-link-target "pkg.go.dev"
        lsp-go-codelenses '((gc_details . t)
                           (generate . t)
                           (regenerate_cgo . t)
                           (tidy . t)
                           (upgrade_dependency . t)
                           (vendor . t)))

  (lsp-register-custom-settings
   '(("gopls.completeUnimported" t t)
     ("gopls.staticcheck" t t)
     ("gopls.analyses.unusedparams" t t)
     ("gopls.analyses.shadow" t t))))
[]
;; Setup function for Go programming mode
(defun miso/go-mode-setup ()
  "Setup for Go programming mode."
  (lsp-deferred)  ; Ensure LSP is started
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t)
  (setq tab-width 4)
  (setq indent-tabs-mode t)
  ;; Go specific keybindings
  (local-set-key (kbd "M-i t") 'go-test-current-test)
  (local-set-key (kbd "M-i f") 'go-test-current-file)
  (local-set-key (kbd "M-i p") 'go-test-current-project))

;; Add the setup hooks
(add-hook 'go-mode-hook #'miso/go-mode-setup)
(add-hook 'go-ts-mode-hook #'miso/go-mode-setup)

;; Set up debugging support
(dap-go-setup)
