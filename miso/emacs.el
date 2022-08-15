;; hide menu / toolbars
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(counsel-mode 1)
(ivy-mode 1)
(doom-modeline-mode 1)
(add-hook 'after-init-hook #'doom-modeline-mode)

;; remove ugly window divider
(window-divider-mode -1)
(setq window-divider-default-bottom-width 0)

;; hide splash screen
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

;; sane defaults for emacs
(defalias 'yes-or-no-p 'y-or-n-p)
(setq make-backup-files nil)
(setq backup-inhibited t)
(setq auto-save-default nil)
(setq temporary-file-directory "/tmp/")
(setq create-lockfiles nil)
(setq auto-save-file-name-transforms
	`((".*" ,temporary-file-directory t)))

(setq confirm-kill-emacs nil)
(set-default 'truncate-lines t)

;; change cursor color
(set-cursor-color "#77B8E9")

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
  
(windmove-default-keybindings 'meta)

(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z")   'undo-fu-only-undo)
(global-set-key (kbd "C-S-z") 'undo-fu-only-redo)
