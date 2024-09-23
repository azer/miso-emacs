;; Set up the snippet directories
(setq yas-snippet-dirs
      '("~/.miso-emacs/snippets"  ; miso snippets
	"~/.miso-emacs/private/snippets"
	yas-installed-snippets-dir)) ; default snippets

;; Enable YASnippet globally
(yas-global-mode 1)

;; Load all snippets
(yas-reload-all)

;; Enable YASnippet in specific modes
(add-hook 'prog-mode-hook #'yas-minor-mode)
(add-hook 'text-mode-hook #'yas-minor-mode)

;; Use hippie-expand as well
(add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand)

;; Allow nested expansions
(setq yas-triggers-in-field t)

;; Wrap around region
(setq yas-wrap-around-region t)

;; Key bindings
(define-key yas-minor-mode-map (kbd "M-i <tab>") #'yas-expand)
;;(define-key yas-minor-mode-map (kbd "M-i Y") #'yas-insert-snippet)
(define-key yas-keymap (kbd "C-h") #'yas-next-field-or-maybe-expand)
(define-key yas-keymap (kbd "C-l") #'yas-prev-field)
