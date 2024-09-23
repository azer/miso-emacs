(message "Setting up dependencies...")

;;(package-initialize)

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package company :ensure t	    ;;
;;   :config				    ;;
;;   (setq company-idle-delay 0)	    ;;
;;   (setq company-minimum-prefix-length 1) ;;
;;   (setq lsp-completion-provider :capf)   ;;
;;   :custom				    ;;
;;     (lsp-enable-snippet t)		    ;;
;;   )					    ;;
;; 					    ;;
;; (use-package company-quickhelp	    ;;
;;   :ensure t				    ;;
;;   :init				    ;;
;;   (company-quickhelp-mode 1)		    ;;
;;   (use-package pos-tip		    ;;
;;     :ensure t))			    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ace-window :ensure t)
;;(use-package counsel :ensure t)

;;(use-package ivy :ensure t)
(use-package swiper :ensure t)
(use-package projectile :ensure t)
;;(use-package counsel-projectile :ensure t)
(use-package writeroom-mode :ensure t)
;;(use-package linum-relative :ensure t)
(use-package undo-fu :ensure t)
(use-package goto-last-change :ensure t)
(use-package flyspell-correct
  :ensure t
  :bind (:map flyspell-mode-map ("M-i c" . flyspell-correct-wrapper)))

(use-package flyspell-correct-ivy :ensure t :after flyspell-correct)

;; The `vertico' package applies a vertical layout to the minibuffer.
;; It also pops up the minibuffer eagerly so we can see the available
;; options without further interactions.  This package is very fast
;; and "just works", though it also is highly customisable in case we
;; need to modify its behaviour.
;;
;; Further reading: https://protesilaos.com/emacs/dotemacs#h:cff33514-d3ac-4c16-a889-ea39d7346dc5
(use-package vertico
  :ensure t
  :config
  (setq vertico-cycle t)
  (setq vertico-resize nil)
  (vertico-mode 1))

;; The `marginalia' package provides helpful annotations next to
;; completion candidates in the minibuffer.  The information on
;; display depends on the type of content.  If it is about files, it
;; shows file permissions and the last modified date.  If it is a
;; buffer, it shows the buffer's size, major mode, and the like.
;;
;; Further reading: https://protesilaos.com/emacs/dotemacs#h:bd3f7a1d-a53d-4d3e-860e-25c5b35d8e7e
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode 1))

;; Ensure system PATH is correctly set in Emacs
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))

;; Selectrum for completion
(use-package selectrum
  :ensure t
  :config
  (selectrum-mode +1))

;; Orderless for flexible matching
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless))
  :config
  (setq selectrum-refine-candidates-function #'orderless-filter)
  (setq selectrum-highlight-candidates-function #'orderless-highlight-matches))

;; Marginalia for minibuffer annotations
(use-package marginalia
  :config
  (marginalia-mode))

;; Consult for enhanced search and navigation
(use-package consult
  :config
  (setq consult-async-split-style nil)
  (setq consult-ripgrep-command "rg -F --null --line-buffered --color=ansi --max-columns=500 --no-heading --line-number . -e ARG OPTS")
  (setq consult-async-min-input 1)
  (setq consult-narrow-key nil)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

(use-package projectile
  :config
  (projectile-mode +1)
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package consult-projectile
  :straight (consult-projectile :type git :host gitlab :repo "OlMon/consult-projectile" :branch "master"))

(use-package embark
  :ensure t
  :bind (("C-." . embark-act)
         :map minibuffer-local-map
         ("C-c C-c" . embark-collect)
         ("C-c C-e" . embark-export)))

;; The `embark-consult' package is glue code to tie together `embark'
;; and `consult'.f
(use-package embark-consult
  :ensure t)

;; The `wgrep' packages lets us edit the results of a grep search
;; while inside a `grep-mode' buffer.  All we need is to toggle the
;; editable mode, make the changes, and then type C-c C-c to confirm
;; or C-c C-k to abort.
;;
;; Further reading: https://protesilaos.com/emacs/dotemacs#h:9a3581df-ab18-4266-815e-2edd7f7e4852
(use-package wgrep
  :ensure t
  :bind ( :map grep-mode-map
          ("e" . wgrep-change-to-wgrep-mode)
          ("C-x C-q" . wgrep-change-to-wgrep-mode)
          ("C-c C-c" . wgrep-finish-edit)))

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
;; (use-package typescript-mode
;;   :after tree-sitter
;;   :ensure t
;;   :mode (("\\.ts\\'" . typescript-mode)
;;          ("\\.tsx\\'" . typescriptreact-mode))
;;   :config
;;   (setq typescript-indent-level 2)
;;   (add-hook 'typescript-mode #'subword-mode)

;;   (define-derived-mode typescriptreact-mode typescript-mode
;;     "TypeScript TSX")

;;   ;; use our derived mode for tsx files
;;   (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescriptreact-mode))
;;   ;; by default, typescript-mode is mapped to the treesitter typescript parser
;;   ;; use our derived mode to map both .tsx AND .ts -> typescriptreact-mode -> treesitter tsx
;;   (add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx)))

;; (use-package eglot
;;   :ensure t
;;   :defer 3
;;   :hook
;;   ((js-mode
;;     typescript-mode
;;     typescriptreact-mode) . eglot-ensure)
;;   :config
;;   (cl-pushnew '((js-mode typescript-mode typescriptreact-mode) . ("typescript-language-server" "--stdio"))
;;               eglot-server-programs
;;               :test #'equal))

(use-package posframe
  :straight t
  :after dashboard
  )

(use-package markdown-mode
  :straight t
  :after dashboard
  )

;; (use-package lsp-bridge
;;   :straight '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
;;             :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
;;             :build (:not compile))
;;   :init
;;   (global-lsp-bridge-mode))



(use-package posframe :ensure t)
(use-package dap-mode :ensure t)
(use-package go-dlv :ensure t)

;; (use-package lsp-ui
;;   :ensure t
;;   :commands lsp-ui-mode
;;   :init
;;   :config
;;   (setq lsp-headerline-breadcrumb-enable t)
;;   (setq lsp-ui-doc-position 'at-point)
;;   (setq lsp-ui-sideline--code-actions t)
;;   :custom
;;   (lsp-ui-doc-position 'bottom)
;;   (lsp-ui-doc-delay 0)
;;   )

;; (use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
;; (use-package lsp-treemacs
;;   :commands lsp-treemacs-errors-list
;;   :custom
;;   (lsp-treemacs-sync-mode 1)
;;   )

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

(use-package flycheck
  :ensure t
  :hook ((typescript-ts-mode . flycheck-mode)
         (tsx-ts-mode . flycheck-mode))
  :config
  (flycheck-add-mode 'typescript-tslint 'typescript-ts-mode)
  (flycheck-add-mode 'typescript-tslint 'tsx-ts-mode)
  )

(use-package consult-flycheck
  :ensure t)

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

;;(use-package devdocs :ensure t)

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

;; Ensure find-file-in-project is installed
(use-package find-file-in-project
  :ensure t)

(use-package web-mode
  :ensure t
  )

(use-package rjsx-mode
  :ensure t
  )

(use-package prettier
  :ensure t
  :hook ((typescript-ts-mode . prettier-mode)
         (tsx-ts-mode . prettier-mode)))

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
  :hook ((typescript-ts-mode . yas-minor-mode)
         (tsx-ts-mode . yas-minor-mode))
  :config
  (add-hook 'typescript-ts-mode-hook
            (lambda () (yas-activate-extra-mode 'typescript-mode)))
  (add-hook 'tsx-ts-mode-hook
            (lambda () (yas-activate-extra-mode 'typescript-mode)))
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
  :ensure t
  :config
  (global-treesit-auto-mode))

(use-package treesit-auto
  :config
  (global-treesit-auto-mode))

(use-package yasnippet
  :ensure t)

(use-package yasnippet-snippets
  :ensure t)

(use-package consult-yasnippet
  :ensure t)

(use-package copilot
  :load-path (lambda () (concat +emacs-dir+ "/copilot.el"))
  ;; don't show in mode line
  :diminish)

(use-package dtrt-indent
  :ensure t)

;;;; Code Completion
(use-package corfu
  :ensure t
  ;; Optional customizations
  :custom
  (corfu-cycle t)                 ; Allows cycling through candidates
  (corfu-auto t)                  ; Enable auto completion
  (corfu-auto-prefix 2)           ; Minimum length of prefix for completion
  (corfu-auto-delay 0)            ; No delay for completion
  (corfu-popupinfo-delay '(0.5 . 0.2))  ; Automatically update info popup after that numver of seconds
  (corfu-preview-current 'insert) ; insert previewed candidate
  (corfu-preselect 'prompt)
  (corfu-on-exact-match nil)      ; Don't auto expand tempel snippets
  ;; Optionally use TAB for cycling, default is `corfu-complete'.
  :bind (:map corfu-map
              ("M-SPC"      . corfu-insert-separator)
              ("TAB"        . corfu-next)
              ([tab]        . corfu-next)
              ("S-TAB"      . corfu-previous)
              ([backtab]    . corfu-previous)
              ("S-<return>" . corfu-insert)
              ("RET"        . corfu-insert))

  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode) ; Popup completion info
  :config
  (add-hook 'eshell-mode-hook
            (lambda () (setq-local corfu-quit-at-boundary t
                                   corfu-quit-no-match t
                                   corfu-auto nil)
              (corfu-mode))
            nil
            t))

;; auto-format different source code files extremely intelligently
;; https://github.com/radian-software/apheleia
(use-package apheleia
  :ensure apheleia
  :diminish ""
  :defines
  apheleia-formatters
  apheleia-mode-alist
[]  :functions
  apheleia-global-mode
  :config
  (setf (alist-get 'prettier-json apheleia-formatters)
        '("prettier" "--stdin-filepath" filepath))
  (apheleia-global-mode +1))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (global-treesit-auto-mode))

(use-package tree-sitter-langs
  :ensure t)
