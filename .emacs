(defconst +home-dir+ "~")
(defconst +emacs-dir+ (concat +home-dir+ "/.miso-emacs"))

(defun add-load-path (p)
  (add-to-list 'load-path (concat +emacs-dir+ "/" p)))

(defun load-local-file (f)
  (load-file (concat +emacs-dir+ "/" f ".el")))

(defun private (name)
  (let ((file (concat +emacs-dir+ "/private/" name ".el")))
    (when (file-exists-p file)
      (load-file file))
    )
  )

(defun include (f)
  (load-file (concat +emacs-dir+ "/" f ".el")))

(defun enable (f)
  (load-file (concat +emacs-dir+ "/config/" f ".el")))

(add-load-path "")
(add-load-path "config")

(private "config")
(include "init")
(private "init")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (magit))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(git-gutter:added ((t (:foreground "#50fa7b" :background "#50fa7b"))))
 '(git-gutter:deleted ((t (:foreground "#ff79c6" :background "#ff79c6"))))
 '(git-gutter:modified ((t (:foreground "#f1fa8c" :background "#f1fa8c"))))
 '(mode-line ((t (:background "#000000" :foreground "#F8F8F2" :box (:line-width 1 :color "#000000"))))))
