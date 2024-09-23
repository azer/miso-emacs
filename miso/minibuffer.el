;; Define a function to exit the minibuffer
(defun miso/minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))



;; Make ESC quit minibuffer
(define-key minibuffer-local-map (kbd "ESC") 'keyboard-escape-quit)
(define-key minibuffer-local-map [escape] 'miso/minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'miso/minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'miso/minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'miso/minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'miso/minibuffer-keyboard-quit)

;; If you're using `vertico`, you might also want to add this:
(with-eval-after-load 'vertico
  (define-key vertico-map [escape] 'miso/minibuffer-keyboard-quit))
