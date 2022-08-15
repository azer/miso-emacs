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
  (message (concat "Enable" f))
  (load-file (concat +emacs-dir+ "/miso/" f ".el")))

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
 '(custom-safe-themes
   '("0466adb5554ea3055d0353d363832446cd8be7b799c39839f387abb631ea0995" "1d5e33500bc9548f800f9e248b57d1b2a9ecde79cb40c0b1398dec51ee820daf" "835868dcd17131ba8b9619d14c67c127aa18b90a82438c8613586331129dda63" "b6a774ac1fada99360ce16b330a4bd6612cf930ed9a8d678022038ecba121d97" "0d01e1e300fcafa34ba35d5cf0a21b3b23bc4053d388e352ae6a901994597ab1" "cd557c5f43e65c78e7b7edd05eb294ed052e20d0f641df200bb99a9f1e2dc214" "aa501163c38b4a3dd9cac061f6889d18ce81c5912dc4ad79be33dba41287690b" "1fdee27fd139f4399bcea79f562f01ddb57e103fefccd11a9a57e832f50a3bbe" default))
 '(doom-modeline-mode nil)
 '(git-gutter:added-sign "+++")
 '(git-gutter:deleted-sign "-")
 '(git-gutter:modified-sign "~")
 '(js2-strict-inconsistent-return-warning nil)
 '(js2-strict-missing-semi-warning nil)
 '(lsp-enable-snippet t)
 '(lsp-ui-doc-delay 0 t)
 '(lsp-ui-doc-position 'bottom t)
 '(magit-auto-revert-mode nil)
 '(package-selected-packages
   '(company-lsp swift-mode use-package dumb-jump ivy-posframe rust-mode cargo-mode doom-themes magit))
 '(writeroom-mode-line t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(git-gutter:added ((t (:foreground "#50fa7b" :background "#50fa7b"))))
 '(git-gutter:deleted ((t (:foreground "#ff79c6" :background "#ff79c6"))))
 '(git-gutter:modified ((t (:foreground "#f1fa8c" :background "#f1fa8c"))))
 '(lsp-ui-doc-header ((t (:background "red" :foreground "black"))))
 '(lsp-ui-doc-highlight-hover ((t (:background "light green"))))
 '(lsp-ui-peek-header ((t (:background "yellow" :foreground "pink" :weight bold))))
 '(markdown-header-face-1 ((t (:foreground "#ACF2E4" :weight bold :height 1.0)))))
