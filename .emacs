(defconst +home-dir+ "~")
(defconst +emacs-dir+ (concat +home-dir+ "/.miso-emacs"))

(defun add-load-path (p)
  (add-to-list 'load-path (concat +emacs-dir+ "/" p)))

(defun load-local-file (f)
  (load-file (concat +emacs-dir+ "/" f ".el")))

(defun private (name)
  (let ((file (concat +emacs-dir+ "/private/" name ".el")))
    (when (file-exists-p file)
      (load-file file))
    )
  )

(defun include (f)
  (load-file (concat +emacs-dir+ "/" f ".el")))

(defun enable (f)
  (message (concat "Enable" f))
  (load-file (concat +emacs-dir+ "/miso/" f ".el")))

(add-load-path "")
(add-load-path "config")

(private "config")
(include "init")
(private "init")
