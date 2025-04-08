(defun writing-mode ()
  "Enable writing mode with focused editing environment."
  (interactive)
  ;; Set font face
  (setq buffer-face-mode-face
        (if (eq system-type 'darwin)
            '(:family "Monaco" :height 170)
          '(:family "dejavu sans mono" :height 150)))
  (buffer-face-mode)

  ;; Enable various modes
  (writeroom-mode 1)
  (blink-cursor-mode)
  (visual-line-mode 1)
  (flyspell-mode)

  ;; Disable some modes
  (git-gutter-mode -1)

  ;; Set various variables
  (setq truncate-lines nil
        hl-line-mode nil
        doom-modeline-enable-word-count t)
  (setq-default line-spacing 5)

  ;; Add window resize hook
  (add-hook 'window-size-change-functions 'on-window-resize))

(defun on-window-resize (frame)
  "Adjust writeroom window on resize."
  (writeroom-fit-window))

(defun writing-block-size ()
  "Calculate the appropriate writing block size based on window width."
  (cond ((< (window-total-width) 80) (- (window-total-width) 10))
        ((< (window-total-width) 160) (- (window-total-width) 30))
        ((< (window-total-width) 300) 130)
        (t 200)))

(defun writeroom-fit-window ()
  "Adjust the visual-fill-column-width based on the window size."
  (interactive)
  (setq visual-fill-column-width (writing-block-size)))

;; Configuration for writeroom-mode
(customize-set-variable 'writeroom-mode-line t)
(setq writeroom-global-effects
      (delq 'writeroom-set-fullscreen writeroom-global-effects))

;; Configuration for initial major mode and Google Translate
(setq initial-major-mode 'markdown-mode)
(setq google-translate-default-source-language "auto")
(setq google-translate-default-target-language "en")

;; Function to get selected text or word at point
(defun get-selected-text (start end)
  "Get the selected text or word at point."
  (if (use-region-p)
      (buffer-substring start end)
    (thing-at-point 'word)))

;; Function to pronounce word at point (simplified)
(defun pronounce-at-point ()
  "Pronounce the word at point using Cambridge Dictionary."
  (interactive)
  (let* ((word (get-selected-text (region-beginning) (region-end)))
         (url (concat "https://dictionary.cambridge.org/dictionary/english/" (s-dashed-words word)))
         (audio-url (with-temp-buffer
                      (call-process "curl" nil t nil "-s" url)
                      (goto-char (point-min))
                      (when (re-search-forward "/media/english/us_pron/[/\\w]+\\.mp3" nil t)
                        (concat "https://dictionary.cambridge.org" (match-string 0))))))
    (when audio-url
      (start-process "pronounce" "*pronounce*" "mplayer" audio-url))))
