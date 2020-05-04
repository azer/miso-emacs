;; enable dap mode
(setq dap-auto-configure-features '(sessions locals controls tooltip))

(setq debugging-mode-server-cmd "make start-remote-dlv")

(defun start-remote-delve ()
  (interactive)
  (setq debugging-mode-server-cmd (read-from-minibuffer "Start Remote DLV: " debugging-mode-server-cmd))
  (projectile-with-default-dir (projectile-project-root)
      ;; (eshell-command (concat debugging-mode-server-cmd " &") t)
      (start-process "delve-remote" "*delve-remote*" "make" "start-remote-dlv")
      )
  (let ((new-buffer (get-buffer-create "*delve-remote*")))
    ;; (switch-to-buffer-other-window new-buffer)
    (message (concat "Executing remote debugging server command: " debugging-mode-server-cmd))))

(defun start-debugging-mode ()
  (interactive)
  (dap-mode t)
  (dap-ui-mode t)
  (dap-tooltip-mode)
  (dap-ui-controls-mode 1)
  (dap-ui-sessions)
  (dap-ui-locals)
  (dap-ui-breakpoints)
  (dap-ui-repl)
  )

(defun stop-debugging-mode ()
  (interactive)
  (dap-delete-all-sessions)
  (dap-breakpoint-delete-all)
  (dap-mode 0)
  (dap-ui-mode 0)
  (dap-ui-controls-mode 0)
  (delete-other-windows)
  (kill-buffer "*dap-ui-repl*")
  (kill-buffer "*dap-ui-breakpoints*")
  (kill-buffer "*dap-ui-locals*")
  (kill-buffer "*dap-ui-sessions*")
  )

(defun debugging-mode ()
  (interactive)
  (if (bound-and-true-p dap-mode)
    (stop-debugging-mode)
  (start-debugging-mode)
  ))


(add-hook 'prog-mode-hook (lambda ()
			    (interactive)
			    (local-set-key (kbd "M-i d") 'debugging-mode)))
