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
   '("3cdd0a96236a9db4e903c01cb45c0c111eb1492313a65790adb894f9f1a33b2d" "75b2a02e1e0313742f548d43003fcdc45106553af7283fb5fad74359e07fe0e2" "013728cb445c73763d13e39c0e3fd52c06eefe3fbd173a766bfd29c6d040f100" "ffafb0e9f63935183713b204c11d22225008559fa62133a69848835f4f4a758c" "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" "88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e" "0466adb5554ea3055d0353d363832446cd8be7b799c39839f387abb631ea0995" "1d5e33500bc9548f800f9e248b57d1b2a9ecde79cb40c0b1398dec51ee820daf" "835868dcd17131ba8b9619d14c67c127aa18b90a82438c8613586331129dda63" "b6a774ac1fada99360ce16b330a4bd6612cf930ed9a8d678022038ecba121d97" "0d01e1e300fcafa34ba35d5cf0a21b3b23bc4053d388e352ae6a901994597ab1" "cd557c5f43e65c78e7b7edd05eb294ed052e20d0f641df200bb99a9f1e2dc214" "aa501163c38b4a3dd9cac061f6889d18ce81c5912dc4ad79be33dba41287690b" "1fdee27fd139f4399bcea79f562f01ddb57e103fefccd11a9a57e832f50a3bbe" default))
 '(doom-modeline-mode nil)
 '(js2-strict-inconsistent-return-warning nil)
 '(js2-strict-missing-semi-warning nil)
 '(lsp-elixir-suggest-specs nil)
 '(package-selected-packages
   '(ellama chatgpt-shell diminish helm-dash marginalia amx vertico golden-ratio solaire-mode prettier-js svelte-mode dockerfile-mode elixir-mode company-lsp swift-mode use-package dumb-jump ivy-posframe rust-mode cargo-mode doom-themes magit))
 '(warning-minimum-level :error)
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
 '(lsp-ui-doc-highlight-hover ((t (:background "light green")))))
