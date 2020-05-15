(message "Setting up dependencies...")

(package-initialize)

(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)

(eval-when-compile
  (add-load-path "use-package")
  (require 'use-package))

(use-package markdown-mode :ensure t)

;; auto-completion with company
(use-package company :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  )

(use-package company-quickhelp
  :ensure t
  :init
  (company-quickhelp-mode 1)
  (use-package pos-tip
    :ensure t))

(use-package ace-window :ensure t)

(use-package ivy :ensure t)
(use-package counsel :ensure t)
(use-package swiper :ensure t)
(use-package projectile :ensure t)
(use-package counsel-projectile :ensure t)
(use-package writeroom-mode :ensure t)
(use-package linum-relative :ensure t)
(use-package undo-tree :ensure t)
(use-package flyspell-correct
  :ensure t
  :bind (:map flyspell-mode-map ("M-i c" . flyspell-correct-wrapper)))

(use-package flyspell-correct-ivy :ensure t :after flyspell-correct)

;; show icons in the ivy list
(use-package all-the-icons-ivy
  :ensure t
  :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup))

;; Go
(use-package go-mode :ensure t)

;; TypeScript
(use-package typescript-mode
  :ensure t
  :config
  (setq typescript-indent-level 2)
  (add-hook 'typescript-mode #'subword-mode))

;; LSP
(use-package lsp-mode :ensure t
  :commands (lsp lsp-deferred)
  :hook (go-mode . lsp-deferred)
  :hook (typescript-mode . lsp-deferred)
  :hook (js-mode . lsp-deferred)
  )

(use-package posframe :ensure t)
(use-package dap-mode :ensure t)
(use-package go-dlv :ensure t)

(use-package company-lsp
  :ensure t
  :commands company-lsp
  )

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :init
  )

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
(use-package counsel-projectile :ensure t)

;; m-x enhancements
(use-package smex :ensure t)

;; better search & replace
(use-package anzu :ensure t)

(use-package rg :ensure t
  :init
  (rg-enable-default-bindings)
  )

;; preview markdown with github
(use-package gh-md :ensure t)

;; generate ToC for markdown
(use-package markdown-toc :ensure t)

(use-package flycheck :ensure t)
(use-package all-the-icons :ensure t)
(use-package doom-modeline :ensure t
  :init (doom-modeline-mode 1))

;; use ivy to correct words
(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map ("M-i c" . flyspell-correct-wrapper)))

;; dictionary
(use-package define-word :ensure t)

;; git
(use-package gitattributes-mode :ensure t :defer t)
(use-package gitconfig-mode :ensure t :defer t)
(use-package gitignore-mode :ensure t :defer t)
(use-package magit
  :ensure t
  :custom
  (magit-auto-revert-mode nil)
  :bind
  ("M-g s" . magit-status))

;; display better diffs
(use-package git-gutter
    :ensure t
    :custom
    (git-gutter:modified-sign "~")		; 
    (git-gutter:added-sign    "+++")		; 
    (git-gutter:deleted-sign  "-")		; 
    :custom-face
    (git-gutter:modified ((t (:foreground "#f1fa8c" :background "#f1fa8c"))))
    (git-gutter:added    ((t (:foreground "#50fa7b" :background "#50fa7b"))))
    (git-gutter:deleted  ((t (:foreground "#ff79c6" :background "#ff79c6"))))
)

;; move where I mean
(use-package mwim
  :ensure t
  :bind
  ("C-a" . mwim-beginning-of-code-or-line)
  ("C-e" . mwim-end-of-code-or-line))

;; Display available keybindings in popup
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :hook (after-init . which-key-mode))

(use-package web-mode
  :ensure t
  )

(use-package rjsx-mode
  :ensure t
)

(use-package prettier-js :ensure t)

;; better editing experience with parenthesis
(use-package smartparens
  :ensure t
  :hook
  (after-init . smartparens-global-mode)
  :config
  (require 'smartparens-config)
  (sp-pair "=" "=" :actions '(wrap))
  (sp-pair "+" "+" :actions '(wrap))
  (sp-pair "<" ">" :actions '(wrap))
  (sp-pair "$" "$" :actions '(wrap)))
