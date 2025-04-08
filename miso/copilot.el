;; Ensure required packages are available
(require 'cl-lib)
(require 's)
(require 'dash)
(require 'editorconfig)

;; Initialize packages if not already installed
(let ((pkg-list '(use-package s dash editorconfig)))
  (package-initialize)
  (when-let ((to-install (seq-filter (lambda (pkg) (not (package-installed-p pkg))) pkg-list)))
    (package-refresh-contents)
    (mapc #'package-install pkg-list)))

;; Make sure editorconfig is properly initialized
(editorconfig-mode 1)

;; Copilot completion functions
(defun miso/copilot-complete-or-accept ()
  "Command that either triggers a completion or accepts one if one is available."
  (interactive)
  (if (copilot--overlay-visible)
      (copilot-accept-completion)
    (copilot-complete)))

(defun miso/setup-copilot-keys ()
  "Set up Copilot key bindings for the current buffer."
  (when (derived-mode-p 'prog-mode)
    (let ((map (make-sparse-keymap)))
      ;; Create key bindings
      (define-key map (kbd "M-C-<next>") #'copilot-next-completion)
      (define-key map (kbd "M-C-<prior>") #'copilot-previous-completion)
      (define-key map (kbd "M-C-<right>") #'copilot-accept-completion-by-word)
      (define-key map (kbd "M-C-<down>") #'copilot-accept-completion-by-line)
      (define-key map (kbd "C-<return>") #'copilot-accept-completion-by-line)
      (define-key map (kbd "M-<return>") #'copilot-accept-completion-by-word)
      (define-key map (kbd "M-y") #'miso/copilot-complete-or-accept)
      ;; Set as buffer-local map
      (setq-local minor-mode-overriding-map-alist
                  (cons (cons 'copilot-mode map)
                        (default-value 'minor-mode-overriding-map-alist))))))

;; Hook setup
(add-hook 'prog-mode-hook #'copilot-mode)
(add-hook 'prog-mode-hook #'miso/setup-copilot-keys)

;; Ensure proper indentation handling
(setq copilot-indent-offset-alist
      '((python-mode . 4)
        (js-mode . 2)
        (typescript-mode . 2)
        (tsx-mode . 2)
        (go-mode . 8)
        (c-mode . 4)
        (c++-mode . 4)))

;; The packages required for it to work are: s, dash, editorconfig (and I also use company, use-package and
;; Emacs built-in cl package here as I find those very helpful). Install them like this:
;; https://robert.kra.hn/posts/2023-02-22-copilot-emacs-setup/
(require 'cl)
(let ((pkg-list '(use-package
		          s
		          dash
		          editorconfig
                  )))
  (package-initialize)
  (when-let ((to-install (map-filter (lambda (pkg _) (not (package-installed-p pkg))) pkg-list)))
    (package-refresh-contents)
    (mapc (lambda (pkg) (package-install pkg)) pkg-list)))

(defun miso/copilot-complete-or-accept ()
  "Command that either triggers a completion or accepts one if one is available."
  (interactive)
  (if (copilot--overlay-visible)
      (copilot-accept-completion)
    (copilot-complete)))

(defun miso/setup-copilot-keys ()
  "Set up Copilot key bindings for the current buffer."
  (when (derived-mode-p 'prog-mode)
    (define-key copilot-mode-map (kbd "M-C-<next>") #'copilot-next-completion)
    (define-key copilot-mode-map (kbd "M-C-<prior>") #'copilot-previous-completion)
    (define-key copilot-mode-map (kbd "M-C-<right>") #'copilot-accept-completion-by-word)
    (define-key copilot-mode-map (kbd "M-C-<down>") #'copilot-accept-completion-by-line)
    (define-key (current-local-map) (kbd "C-<return>") #'copilot-accept-completion-by-line)
    (define-key (current-local-map) (kbd "M-<return>") #'copilot-accept-completion-by-word)
    (define-key (current-local-map) (kbd "M-y") #'miso/copilot-complete-or-accept)))

;; (dolist (mode-indent miso/mode-indent-alist)
;;   (let* ((mode (car mode-indent))
;;          (indent-info (cdr mode-indent))
;;          (indent (if (listp indent-info) (car indent-info) indent-info)))
;;     (add-to-list 'copilot-indentation-alist (cons mode indent))))

(add-hook 'prog-mode-hook #'copilot-mode)
(add-hook 'prog-mode-hook #'miso/setup-copilot-keys)
