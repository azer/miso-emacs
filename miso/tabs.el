;; Enable dtrt-indent for automatic indentation style detection
(dtrt-indent-global-mode 1)

;; Configure dtrt-indent
(setq dtrt-indent-verbosity 0)  ; Reduce verbosity of messages
(setq dtrt-indent-min-quality 80.0)  ; Set minimum quality for indentation guessing

;; Define default tab settings for various programming modes
(setq miso/mode-indent-alist
      '((c-mode . (4 . nil))
        (c++-mode . (4 . nil))
        (java-mode . (4 . nil))
        (js-mode . (2 . nil))
        (typescript-mode . (2 . nil))
        (python-mode . (4 . nil))
        (ruby-mode . (2 . nil))
        (rust-mode . (4 . nil))
        (go-mode . (8 . t))
        (elm-mode . (4 . nil))
        (haskell-mode . (2 . nil))
        (sh-mode . (2 . nil))
        (lisp-mode . (2 . nil))
        (emacs-lisp-mode . (2 . nil))
        (yaml-mode . (2 . nil))
        (json-mode . (2 . nil))
        (html-mode . (2 . nil))
        (css-mode . (2 . nil))
        (scss-mode . (2 . nil))
        (web-mode . (2 . nil))))

(defun miso/set-indentation-hook ()
  "Set indentation based on mode-specific defaults if not auto-detected."
  (let* ((mode-indent (cdr (assoc major-mode miso/mode-indent-alist)))
         (indent-width (car mode-indent))
         (use-tabs (cdr mode-indent)))
    (when mode-indent
      (unless (dtrt-indent-try-set-offset)
        (miso/set-indent-internal indent-width use-tabs)))))

(defun miso/set-indent-internal (width use-tabs)
  "Set indentation to WIDTH and use tabs if USE-TABS is non-nil."
  (setq-local tab-width width)
  (setq-local indent-tabs-mode use-tabs)
  (when (derived-mode-p 'prog-mode)
    (setq-local standard-indent width)))

(defun miso/set-indent-as-tab ()
  "Set indentation to use tabs."
  (interactive)
  (miso/set-indent-internal tab-width t)
  (message "Indentation set to tabs"))

(defun miso/set-indent-as-space (num-spaces)
  "Set indentation to use NUM-SPACES spaces."
  (interactive "nNumber of spaces: ")
  (miso/set-indent-internal num-spaces nil)
  (message "Indentation set to %d spaces" num-spaces))

;; Apply the indentation hook to all programming modes
(add-hook 'prog-mode-hook #'miso/set-indentation-hook)

;; Additional mode-specific hooks
(add-hook 'yaml-mode-hook #'miso/set-indentation-hook)
(add-hook 'json-mode-hook #'miso/set-indentation-hook)
(add-hook 'web-mode-hook #'miso/set-indentation-hook)

;; Ensure tabs are visible
(setq whitespace-style '(face tabs tab-mark trailing))
(global-whitespace-mode 1)

(defun miso/get-current-indentation ()
  "Get the current indentation settings."
  (interactive)
  (message "Current indentation: %s"
           (if indent-tabs-mode
               "Tabs"
             (format "%d spaces" tab-width))))

(defun miso/set-indentation ()
  "Set the indentation for the current buffer using consult."
  (interactive)
  (let* ((current-indent (if indent-tabs-mode
                             "Tabs"
                           (format "%d spaces" tab-width)))
         (options '("Tabs" "2 spaces" "4 spaces" "8 spaces"))
         (selection (consult--read
                     options
                     :prompt "Select indentation: "
                     :default current-indent
                     :category 'indentation)))
    (pcase selection
      ("Tabs" (miso/set-indent-as-tab))
      ("2 spaces" (miso/set-indent-as-space 2))
      ("4 spaces" (miso/set-indent-as-space 4))
      ("8 spaces" (miso/set-indent-as-space 8)))))
