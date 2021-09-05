(defun writing-mode ()
  (interactive)
  (setq buffer-face-mode-face '(:family "dejavu sans mono" :height 150))
  (if (eq system-type 'darwin)
      (setq buffer-face-mode-face '(:family "Monaco" :height 170))
      )
  (buffer-face-mode)
  (linum-mode 0)
  (writeroom-mode 1)
  (blink-cursor-mode)
  (visual-line-mode 1)
  (git-gutter-mode -1)
  (flyspell-mode)
  (setq truncate-lines nil)
  (setq-default line-spacing 5)
  (setq global-hl-line-mode nil)
  (setq doom-modeline-enable-word-count t)

  (local-set-key (kbd "M-=") 'writeroom-fit-window)
  (local-set-key (kbd "M-i d") 'define-word-at-point)
  (local-set-key (kbd "M-i i") 'markdown-toc-generate-or-refresh-toc)
  (local-set-key (kbd "M-i j") 'counsel-semantic-or-imenu)
  (local-set-key (kbd "M-i w") 'write)
  (local-set-key (kbd "M-i t") 'google-translate-at-point)
  (local-set-key (kbd "M-i p") 'pronounce-at-point)
  (local-set-key (kbd "M-i <up>") 'magit-push-current-to-upstream)
  (local-set-key (kbd "M-i <down>") 'magit-pull-from-upstream)
  (add-hook 'window-size-change-functions 'on-window-resize)
  )

(defun on-window-resize (frame)
  (writeroom-fit-window))



(customize-set-variable 'writeroom-mode-line t)

(setq initial-major-mode 'markdown-mode)

(setq writeroom-global-effects
  (delq 'writeroom-set-fullscreen writeroom-global-effects))`


(message "writing mode")
(setq google-translate-default-source-language "auto")  ; Auto detect language.
(setq google-translate-default-target-language "en")

(defun pronounce-at-point ()
  (interactive)
  (setq selection-to-pronounce (get-selected-text (region-beginning)
						  (region-end)))
  (message "Looking up pronounciation of %s" selection-to-pronounce)
  (setq cambridge-audio-url
	(substring
	 (shell-command-to-string (concat "curl -s \"https://dictionary.cambridge.org/dictionary/english/" (s-dashed-words selection-to-pronounce) "\" | rg \"/media/english/us_pron/[/\\w]+\\.mp3\" -o | uniq | head -n 1"))
	 0 -1))
  (start-process "delve-remote" "*delve-remote*" "mplayer" (concat "https://dictionary.cambridge.org" cambridge-audio-url))
  )

(defun get-selected-text (start end)
  (interactive "r")
  (if (use-region-p)
      (let ((regionp (buffer-substring start end)))
	(message regionp))
    (thing-at-point 'word))
  )

(defun writing-block-size ()
  (if (< (window-total-width) 80)
      (- (window-total-width) 10)
    (if (< (window-total-width) 160)
	(- (window-total-width) 30)
      (if (< (window-total-width) 300)
	  130
	200
	)))
  )


(defun writeroom-fit-window ()
  (interactive)
  (message (number-to-string (window-total-width)))
  (message (number-to-string (writing-block-size)))
  (setq visual-fill-column-width (writing-block-size))
  )
