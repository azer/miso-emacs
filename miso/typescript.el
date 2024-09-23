;; Auto-mode settings for TypeScript and TSX files
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))

;; Flycheck configuration for TypeScript
(with-eval-after-load 'flycheck
  (flycheck-add-mode 'typescript-tslint 'typescript-ts-mode)
  (flycheck-add-mode 'typescript-tslint 'tsx-ts-mode))

;; LSP configuration for TypeScript
(with-eval-after-load 'lsp-mode
  (add-hook 'typescript-ts-mode-hook #'lsp-deferred)
  (add-hook 'tsx-ts-mode-hook #'lsp-deferred)
  (setq lsp-typescript-suggest-complete-function-calls t)
  (setq lsp-typescript-surveys-enabled nil))

;; YASnippet configuration for TypeScript
(with-eval-after-load 'yasnippet
  (add-hook 'typescript-ts-mode-hook #'yas-minor-mode)
  (add-hook 'tsx-ts-mode-hook #'yas-minor-mode)
  (add-hook 'typescript-ts-mode-hook
            (lambda () (yas-activate-extra-mode 'typescript-mode)))
  (add-hook 'tsx-ts-mode-hook
            (lambda () (yas-activate-extra-mode 'typescript-tsx-mode))))

;; Prettier configuration (if prettier package is available)
(with-eval-after-load 'prettier
  (add-hook 'typescript-ts-mode-hook #'prettier-mode)
  (add-hook 'tsx-ts-mode-hook #'prettier-mode))

;; Additional TypeScript-specific settings
(defun miso/typescript-mode-hook ()
  "Custom hook for TypeScript modes."
  (setq-local typescript-indent-level 2)
  (setq-local indent-tabs-mode nil)
  (setq-local js-indent-level 2))

(add-hook 'typescript-ts-mode-hook #'miso/typescript-mode-hook)
(add-hook 'tsx-ts-mode-hook #'miso/typescript-mode-hook)

;; Optional: Organize imports on save (if lsp-mode is available)
;; (add-hook 'before-save-hook #'lsp-organize-imports)

;; Tree-sitter configuration
(with-eval-after-load 'treesit
  (add-to-list 'treesit-language-source-alist
               '(typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")))
  (add-to-list 'treesit-language-source-alist
               '(tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))))
