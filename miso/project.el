(projectile-mode +1)

(defun miso/list-directories (root)
  "Return a list of all directories under ROOT."
  (let ((cmd (format "find %s -type d" (shell-quote-argument root))))
    (mapcar (lambda (dir)
              (file-name-as-directory (expand-file-name dir root)))
            (split-string (shell-command-to-string cmd) "\n" t))))

(defun miso/find-or-create-file (directory)
  "Find or create a file in DIRECTORY using consult. Excludes .git directory from the search results."
  (let* ((default-directory (expand-file-name directory))
         (files (directory-files-recursively default-directory "" t))
         (files-filtered (seq-filter
                          (lambda (file)
                            (not (string-match-p "/\\.git/" file)))
                          files)))
    (find-file
     (let ((selected (consult--read
                      (mapcar (lambda (path) (file-relative-name path default-directory))
                              files-filtered)
                      :prompt "Find or create file: "
                      :sort nil
                      :require-match nil  ; Allow non-existing files
                      :category 'file)))
       (expand-file-name selected default-directory)))))

(defun miso/jump-to-directory (root)
  "Jump to a directory under ROOT and create/visit a file."
  (interactive)  ; Make sure this is interactive
  (let* ((directories (miso/list-directories root))
         (has-subdirs (> (length directories) 1)))
    (if has-subdirs
        (let* ((selected-dir (consult--read
                              directories
                              :prompt "Select directory: "
                              :category 'file
                              :sort nil
                              :require-match t
                              :state (consult--file-preview)
                              :history 'file-name-history
                              :default (file-name-as-directory root)))
               (default-directory selected-dir))
          (call-interactively 'find-file))
      (let ((default-directory root))
        (call-interactively 'find-file)))))

(defun miso/jump-to-file ()
  "Jump to a directory and create/visit a file in the project or current directory."
  (interactive)  ; Ensure this is marked as interactive
  (if (projectile-project-p)
      (miso/jump-to-directory (projectile-project-root))
    (let ((current-dir (if buffer-file-name
                           (file-name-directory buffer-file-name)
                         default-directory)))
      (miso/jump-to-directory current-dir))))

(defun miso/project-ripgrep ()
  "Run ripgrep in the current project or current directory using consult-ripgrep."
  (interactive)
  (let ((search-dir (or (projectile-project-root)
                        (and buffer-file-name (file-name-directory buffer-file-name))
                        default-directory)))
    (consult-ripgrep search-dir)))

(defvar miso/command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "f") 'projectile-find-file)
    (define-key map (kbd "p") 'projectile-switch-project)
    (define-key map (kbd "b") 'projectile-switch-to-buffer)
    (define-key map (kbd "s") 'miso/project-ripgrep)
    (define-key map (kbd "r") 'projectile-replace-regexp)
    (define-key map (kbd "n") 'miso/jump-to-file)
    map)
  "Keymap for Miso custom commands.")

(global-set-key (kbd "M-p") miso/command-map)
