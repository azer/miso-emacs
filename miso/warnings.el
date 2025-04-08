;; Prevent *Warnings* buffer from popping up
(setq warning-minimum-level :error)  ; Only show errors, not warnings

;; Configure display-warning behavior
(setq display-warning-minimum-level :error)

;; Use mini-buffer for warnings instead of separate buffer
(setq warning-suppress-types nil)
(setq warning-suppress-log-types nil)
(setq warning-fill-prefix "    ")

;; Custom function to display warnings in echo area
(defun display-warning-echo-area (type message &optional level buffer-name)
  "Display a warning message in the echo area instead of a buffer."
  (message (format "[%s] %s" type message)))

;; Override the default warning display function
(setq display-warning-function #'display-warning-echo-area)

;; Prevent warnings buffer from splitting windows
(add-to-list 'display-buffer-alist
             '("\\*Warnings\\*"
               (display-buffer-no-window)
               (allow-no-window . t)))
