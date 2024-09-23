;; New and modified functions
(defun miso/get-search-dir ()
  "Get the appropriate search directory."
  (or (projectile-project-root)
      (and buffer-file-name (file-name-directory buffer-file-name))
      default-directory))

(defun miso/consult-find ()
  "Run consult-find in the current project or current directory."
  (interactive)
  (let ((search-dir (miso/get-search-dir)))
    (consult-find search-dir)))

(defun miso/consult-ripgrep ()
  "Run consult-ripgrep in the current project or current directory."
  (interactive)
  (let ((search-dir (miso/get-search-dir)))
    (consult-ripgrep search-dir)))

;; shortcuts
(global-set-key "\C-s" 'consult-line)
(global-set-key (kbd "M-<tab>") 'consult-buffer)
(global-set-key (kbd "C-x b") 'consult-buffer)
