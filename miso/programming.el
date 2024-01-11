(add-hook 'prog-mode-hook 'linum-relative-mode)
(add-hook 'prog-mode-hook 'rabbit-mode)
(add-hook 'prog-mode-hook 'company-mode)
(add-hook 'prog-mode-hook 'git-gutter-mode)
(add-hook 'prog-mode-hook 'show-paren-mode)

(add-hook 'conf-mode-hook 'linum-relative-mode)
(add-hook 'conf-mode-hook 'rabbit-mode)
(add-hook 'conf-mode-hook 'company-mode)
(add-hook 'conf-mode-hook 'git-gutter-mode)
(add-hook 'conf-mode-hook 'show-paren-mode)

(add-hook 'prog-mode-hook (lambda ()
			    (interactive)
			    (hl-line-mode)
			    (set-face-attribute 'linum nil :family "Menlo" :height 140 :slant 'normal)
			    (set-face-attribute 'linum-relative-current-face nil :family "Menlo" :height 140 :slant 'normal)
			    (local-set-key (kbd "M-\\") 'company-yasnippet)
			    (local-set-key (kbd "M-i e") 'counsel-flycheck)
			    (local-set-key (kbd "M-i f") 'lsp-execute-code-action)
			    (local-set-key (kbd "M-i r") 'lsp-rename)
			    (local-set-key (kbd "M-i j") 'counsel-semantic-or-imenu)
			    (local-set-key (kbd "M-i w") 'write)
			    (local-set-key (kbd "M-i i") 'lsp-organize-imports)
			    (local-set-key (kbd "M-i g") 'lsp-find-definition)
			    (local-set-key (kbd "M-i t") 'lsp-goto-type-definition)
			    (local-set-key (kbd "M-i d") 'dumb-jump-go)
			    (local-set-key (kbd "M-i c") 'lsp-find-references)
			    (local-set-key (kbd "M-i l") 'goto-last-change)
			    (local-set-key (kbd "M-i -") 'revert-buffer)
			    (local-set-key (kbd "M-i <up>") 'magit-push-current-to-upstream)
			    (local-set-key (kbd "M-i <down>") 'magit-pull-from-upstream)
			    ))

'(lsp-headerline-breadcrumb-enable-symbol-numbers nil)
'(lsp-headerline-breadcrumb-icons-enable nil)

(eval-after-load "auto-complete"
  '(add-to-list 'ac-sources 'ac-source-yasnippet))

(defvar counsel-flycheck-history nil
  "history for `counsel-flycheck'")

(defun counsel-flycheck ()
  (interactive)
  (if (not (bound-and-true-p flycheck-mode))
      (message "Flycheck mode is not available or enabled")
    (ivy-read "Error: "
              (let ((source-buffer (current-buffer)))
                (with-current-buffer
                    (or (get-buffer flycheck-error-list-buffer)
                        (progn
                          (with-current-buffer
                              (get-buffer-create flycheck-error-list-buffer)
                            (flycheck-error-list-mode)
                            (current-buffer))))
                  (flycheck-error-list-set-source source-buffer)
                  (flycheck-error-list-reset-filter)
                  (revert-buffer t t t)
                  (split-string (buffer-string) "\n" t " *")))
              :action (lambda (s &rest _)
                        (-when-let* ( (error (get-text-property 0 'tabulated-list-id s))
                                      (pos (flycheck-error-pos error)) )
                          (goto-char (flycheck-error-pos error))))
              :history 'counsel-flycheck-history
              :caller 'counsel-flycheck)))


;; quit after hitting enter in find references xref window
(defun my/do-then-quit (&rest args)
  (let ((win (selected-window)))
    (apply (car args) (rest args))
    (quit-window nil win)))

(advice-add #'xref-goto-xref :around #'my/do-then-quit)

(eval-after-load "linum"
  '(progn
  (set-face-attribute 'linum nil :family "Menlo" :height 140 :slant 'normal)
  (set-face-attribute 'linum-relative-current-face nil :family "Menlo" :height 140 :slant 'normal)
  ))
