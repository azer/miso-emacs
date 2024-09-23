(defun miso/git-commit-all ()
  "Stage all changes and create a commit."
  (interactive)
  (magit-stage-modified t)
  (magit-commit-create))

(defvar miso/git-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "b") 'magit-blame-echo)
    (define-key map (kbd "f") 'magit-diff-buffer-file)
    (define-key map (kbd "d") 'magit-diff-working-tree)
    (define-key map (kbd "s") 'magit-status)
    (define-key map (kbd "c") 'magit-commit-create)
    (define-key map (kbd "C") 'miso/git-commit-all)
    (define-key map (kbd "<up>") 'magit-push-current-to-upstream)
    (define-key map (kbd "<down>") 'magit-pull-from-upstream)
    map)
  "Keymap for Miso git commands.")

(global-set-key (kbd "M-g") miso/git-command-map)
