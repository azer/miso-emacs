;; Display word count in writing mode
(setq doom-modeline-continuous-word-count-modes '(markdown-mode))

;; buffer encoding
(setq doom-modeline-buffer-encoding nil)

;; turn off percentage
(setq doom-modeline-percent-position nil)

(setq doom-modeline-major-mode nil)

(set-face-attribute 'doom-modeline-info nil
                    :background "#000000"
                    :foreground "#888888"
                    :box nil
                    :overline nil
                    :underline nil)

(set-face-attribute 'doom-modeline-bar nil
                    :background "#ffdf6b" ;;
                    :foreground "#000000"
		    :box nil
                    :overline nil
                    :underline nil)

(set-face-attribute 'doom-modeline-bar-inactive nil
                    :background "#333333"
                    :foreground "#00ffcc"
		    :box nil
                    :overline nil
                    :underline nil)

(set-face-attribute 'doom-modeline-panel nil
                    :background "#333333"
                    :foreground "#00ffcc"
		    :box nil
                    :overline nil
                    :underline nil)


(set-face-attribute 'doom-modeline-lsp-success nil
                    :background "#000000"
                    :foreground "#333333"
		    :box nil
                    :overline nil
                    :underline nil)

(set-face-attribute 'mode-line nil
                    :background "#"
                    :foreground "#f8f8f2"
		    :box nil
                    :overline nil
                    :underline nil)

(set-face-attribute 'mode-line-highlight nil
                    :background "#000000"
                    :foreground "#f8f8f2"
		    :box nil
                    :overline nil
                    :underline nil)
