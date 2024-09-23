;;; programming.el --- Programming mode configurations for miso-emacs

;;; Commentary:
;; This file contains configurations for programming modes in miso-emacs.

;;; Code:

;; General programming mode hooks
(defun miso/prog-mode-setup ()
  "Setup for programming modes."
  (interactive)
  (hl-line-mode)
  (copilot-mode)
  ;;(lsp-mode)
  (miso/set-prog-mode-keybindings))

(defun miso/set-prog-mode-keybindings ()
  "Set keybindings for programming modes."
  (local-set-key (kbd "M-\\") 'consult-yasnippet)
  (local-set-key (kbd "M-i e") 'consult-flycheck)
  (local-set-key (kbd "M-i a") 'lsp-execute-code-action)
  (local-set-key (kbd "M-i r") 'lsp-rename)
  (local-set-key (kbd "M-i j") 'consult-imenu)
  (local-set-key (kbd "M-i w") 'write)
  (local-set-key (kbd "M-i i") 'lsp-organize-imports)
  (local-set-key (kbd "M-i g") 'lsp-find-definition)
  (local-set-key (kbd "M-i t") 'lsp-goto-type-definition)
  ;;(local-set-key (kbd "M-i d") 'dumb-jump-go)
  (local-set-key (kbd "M-i d") 'lsp-find-references)
  (local-set-key (kbd "M-i l") 'goto-last-change)
  (local-set-key (kbd "M-i -") 'revert-buffer)
  (local-set-key (kbd "M-i f") 'apheleia-format-buffer)
  ;;(local-set-key (kbd "M-i .") 'devdocs-lookup)
  (local-set-key (kbd "M-i y") 'consult-yasnippet)
  (local-set-key (kbd "M-i <up>") 'magit-push-current-to-upstream)
  (local-set-key (kbd "M-i <down>") 'magit-pull-from-upstream))

;; Hook setup for programming and configuration modes
(defun miso/add-common-hooks (mode-hook)
  "Add common hooks to MODE-HOOK."
  (add-hook mode-hook 'rabbit-mode)
  ;; (add-hook mode-hook 'company-mode)
  ;; (add-hook mode-hook 'git-gutter-mode)  ;; Commented out as in original
  (add-hook mode-hook 'show-paren-mode))

(miso/add-common-hooks 'prog-mode-hook)
(miso/add-common-hooks 'conf-mode-hook)
(add-hook 'prog-mode-hook 'miso/prog-mode-setup)

;; Auto-complete configuration
;;(eval-after-load "auto-complete"
;;  '(add-to-list 'ac-sources 'ac-source-yasnippet))

;; LSP performance improvements
(setq load-prefer-newer t)
(setq large-file-warning-threshold (* 64 1000000))
(setq undo-outer-limit (* 64 1000000))
(setq read-process-output-max (* 1024 1024))  ; 1 MB

;; XRef configuration
(defun miso/do-then-quit (&rest args)
  "Execute function and quit window."
  (let ((win (selected-window)))
    (apply (car args) (cdr args))
    (quit-window nil win)))

(advice-add #'xref-goto-xref :around #'miso/do-then-quit)

;; Line number display configuration
(setq display-line-numbers-type 'relative)
(setq display-line-numbers-width 3)
(setq display-line-numbers-grow-only t)
(setq display-line-numbers-width-start t)

;;(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(setq treesit-auto-install 'prompt)
