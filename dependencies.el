(message "Setting up dependencies...")

(package-initialize)

(setq package-archives
      '(("melpa" . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/melpa/")
        ("org"   . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/org/")
        ("gnu"   . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/gnu/")))


(eval-when-compile
  (add-load-path "use-package")
  (require 'use-package))

(use-package exec-path-from-shell :ensure t
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  )

(use-package markdown-mode :ensure t)

(use-package solaire-mode :ensure t)

(use-package golden-ratio :ensure t
  :hook (after-init . golden-ratio-mode)
  )

(use-package treemacs :ensure t
  :bind ("<f5>" . treemacs)
  :custom
  (treemacs-is-never-other-window)
  :hook
  (treemacs-mode . treemacs-project-follow-mode)
  )

;; The Emacs default split doesn't seem too intuitive for most users.
(use-package emacs
  :ensure nil
  :preface
  (defun ian/split-and-follow-horizontally ()
    "Split window below."
    (interactive)
    (split-window-below)
    (other-window 1))
  (defun ian/split-and-follow-vertically ()
    "Split window right."
    (interactive)
    (split-window-right)
    (other-window 1))
  :config
  (global-set-key (kbd "C-x 2") #'ian/split-and-follow-horizontally)
  (global-set-key (kbd "C-x 3") #'ian/split-and-follow-vertically))

(use-package diminish
  :ensure t
  :demand t)

(use-package ediff
  :ensure nil
  :config
  (setq ediff-window-setup-function #'ediff-setup-windows-plain)
  (setq ediff-split-window-function #'split-window-horizontally))

(use-package frame
  :preface
  :ensure nil
  :config
  (setq initial-frame-alist '((fullscreen . maximized))))

;; Replace the active region just by typing text, just like modern editors.
(use-package delsel
  :ensure nil
  :config (delete-selection-mode +1))

;; auto-completion with company
(use-package company :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (setq lsp-completion-provider :capf)
  :custom
    (lsp-enable-snippet t)
  )

(use-package company-quickhelp
  :ensure t
  :init
  (company-quickhelp-mode 1)
  (use-package pos-tip
    :ensure t))

(use-package ace-window :ensure t)
(use-package counsel :ensure t)

(use-package ivy :ensure t)
(use-package swiper :ensure t)
(use-package projectile :ensure t)
(use-package counsel-projectile :ensure t)
(use-package writeroom-mode :ensure t)
(use-package linum-relative :ensure t)
(use-package undo-fu :ensure t)
(use-package goto-last-change :ensure t)
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

;; Elixir
(use-package elixir-mode :ensure t)

(use-package dockerfile-mode :ensure t)

;; rust
(use-package cargo-mode :ensure t)
(use-package rust-mode :ensure t
  :config
  (add-hook 'rust-mode-hook
	    (lambda () (setq indent-tabs-mode nil)))
  (setq rust-format-on-save t)
  (add-hook 'rust-mode-hook
            (lambda () (prettify-symbols-mode)))
  (add-hook 'rust-mode-hook #'lsp)
  )

;; TypeScript
(use-package typescript-mode
  :after tree-sitter
  :ensure t
  :config
  (setq typescript-indent-level 2)
  (add-hook 'typescript-mode #'subword-mode)

  (define-derived-mode typescriptreact-mode typescript-mode
    "TypeScript TSX")

  ;; use our derived mode for tsx files
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescriptreact-mode))
  ;; by default, typescript-mode is mapped to the treesitter typescript parser
  ;; use our derived mode to map both .tsx AND .ts -> typescriptreact-mode -> treesitter tsx
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx)))

;; LSP
(use-package lsp-mode :ensure t
  :commands (lsp lsp-deferred)
  :hook (go-mode . lsp-deferred)
  :hook (elixir-mode . lsp-deferred)
  :hook (typescript-mode . lsp-deferred)
  :hook (js-mode . lsp-deferred)
  )

(use-package posframe :ensure t)
(use-package dap-mode :ensure t)
(use-package go-dlv :ensure t)

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :init
  :config
  (setq lsp-headerline-breadcrumb-enable t)
  (setq lsp-ui-doc-position 'at-point)
  (setq lsp-ui-sideline--code-actions t)
  :custom
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-delay 0)
  )

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list
  :custom
  (lsp-treemacs-sync-mode 1)
  )

;; m-x enhancements
(use-package smex :ensure t)

;; better search & replace
(use-package anzu :ensure t)

(use-package ctrlf :ensure t)

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

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config (column-number-mode 1)
  :custom
  (doom-modeline-height 30)
  (doom-modeline-window-width-limit nil)
  (doom-modeline-buffer-file-name-style 'relative-from-project)
  (doom-modeline-minor-modes nil)
  (doom-modeline-enable-word-count t)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-env-python-executable "python")
  ;;needs display-time-mode to be one
  (doom-modeline-time t)

  (doom-modeline-vcs-max-length 100))


(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;;(load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  ;;(setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; use ivy to correct words
(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map ("M-i c" . flyspell-correct-wrapper)))

;; dictionary
(use-package define-word :ensure t)

(use-package popup :ensure t)

;; translate
(use-package google-translate :ensure t
  :config
  (require 'google-translate)
  (require 'google-translate-default-ui)
  )

;; git
(use-package git-modes :ensure t :defer t)
(use-package magit
  :ensure t
  :custom
  (magit-auto-revert-mode nil)
  :bind
  ("M-g g" . magit-status)
  ("M-g b" . 'magit-diff-buffer-file)
  ("M-g d" . 'magit-diff-working-tree)
  ("M-g -" . 'magit-stash-worktree)
  ("M-g =" . 'magit-stash-apply)
  ("M-g c" . 'magit-commit-worktree)
  ("M-g s" . 'magit-stage-modified)
  ("M-g /" . 'counsel-git-checkout)
  ("M-g l" . 'magit-blame-echo)
  )

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

(use-package devdocs :ensure t)

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

(use-package prettier :ensure t)

(use-package yaml-mode :ensure t)

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

(use-package yasnippet
  :ensure t
  :init (yas-global-mode 1)
  )

(use-package calfw
  :ensure t
  )

(use-package calfw-org
  :ensure t
  )

(use-package dumb-jump
  :ensure t
  )

(use-package treesit
  :commands (treesit-install-language-grammar nf/treesit-install-all-languages)
  :init
  (setq treesit-language-source-alist
   '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
     (c . ("https://github.com/tree-sitter/tree-sitter-c"))
     (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
     (css . ("https://github.com/tree-sitter/tree-sitter-css"))
     (cmake . ("https://github.com/uyha/tree-sitter-cmake"))
     (go . ("https://github.com/tree-sitter/tree-sitter-go"))
     (html . ("https://github.com/tree-sitter/tree-sitter-html"))
     (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
     (json . ("https://github.com/tree-sitter/tree-sitter-json"))
     (julia . ("https://github.com/tree-sitter/tree-sitter-julia"))
     (lua . ("https://github.com/Azganoth/tree-sitter-lua"))
     (make . ("https://github.com/alemuller/tree-sitter-make"))
     (ocaml . ("https://github.com/tree-sitter/tree-sitter-ocaml" "master" "ocaml/src"))
     (python . ("https://github.com/tree-sitter/tree-sitter-python"))
     (php . ("https://github.com/tree-sitter/tree-sitter-php"))
     (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
     (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
     (ruby . ("https://github.com/tree-sitter/tree-sitter-ruby"))
     (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
     (sql . ("https://github.com/m-novikov/tree-sitter-sql"))
     (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))
     (zig . ("https://github.com/GrayJack/tree-sitter-zig"))))
  :config
  (defun nf/treesit-install-all-languages ()
    "Install all languages specified by `treesit-language-source-alist'."
    (interactive)
    (let ((languages (mapcar 'car treesit-language-source-alist)))
      (dolist (lang languages)
	      (treesit-install-language-grammar lang)
	      (message "`%s' parser was installed." lang)
	      (sit-for 0.75)))))

(use-package treesit-auto
  :config
  (global-treesit-auto-mode))

(use-package ellama
  :ensure t
  :init
  (setopt ellama-language "English")
  (require 'llm-ollama)
  (setopt ellama-provider
	  (make-llm-ollama
	   :chat-model "mistral:latest" :embedding-model "mistral:latest"))
  ;; Predefined llm providers for interactive switching.
  ;; You shouldn't add ollama providers here - it can be selected interactively
  ;; without it. It is just example.
  (setopt ellama-providers
	  '(("mistral" . (make-llm-ollama
			  :chat-model "mistral:latest"
			  :embedding-model "mistral:latest"))
	    ("zephyr" . (make-llm-ollama
			 :chat-model "zephyr:7b-beta-q6_K"
			 :embedding-model "zephyr:7b-beta-q6_K"))
	    ("mixtral" . (make-llm-ollama
			  :chat-model "mixtral:8x7b-instruct-v0.1-q3_K_M-4k"
			  :embedding-model "mixtral:8x7b-instruct-v0.1-q3_K_M-4k")))))
