;; enable doom modeline
(doom-modeline-mode)

;; Display word count in writing mode
(setq doom-modeline-continuous-word-count-modes '(markdown-mode))

;; buffer encoding
(setq doom-modeline-buffer-encoding nil)

;; turn off percentage
(setq doom-modeline-percent-position nil)

(setq doom-modeline-major-mode nil)

(custom-set-faces
  '(mode-line ((t (:family "Inconsolata" :height 150 :weight normal))))
  '(mode-line-active ((t (:family "Inconsolata" :height 150 :weight normal)))) ; For 29+
  '(mode-line-inactive ((t (:family "Inconsolata" :height 150 :weight normal)))))
