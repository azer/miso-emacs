;; scratch-mode
(setq initial-scratch-message (concat "\n\n# " (format-time-string "%Y-%m-%d") "\n "))

(defun write ()
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (delete-other-windows)
  (writing-mode)
  )
