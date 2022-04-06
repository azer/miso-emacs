;; hide menu / toolbars
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(counsel-mode 1)
(ivy-mode 1)


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
