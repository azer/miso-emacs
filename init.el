(message "Setting up Misomacs")

;; make sure dependencies are installed
(include "dependencies")

(enable "emacs")
(enable "completion")
(enable "writing-mode")
(enable "markdown")
(enable "rabbit-mode")
(enable "trailing-whitespace")
(enable "rename-current-buffer")
(enable "programming")
(enable "go")
(enable "typescript")
(enable "javascript")
(enable "project")
(enable "anzu")
(enable "modeline")
(enable "scratch")
(enable "debugging")
(enable "windows")
(enable "git")
(enable "mac")
(enable "colors")

(set-cursor-color "#77B8E9")
(setq cursor-type '(bar . 8))


(set-face-attribute 'default nil
                    :family "Menlo"
                    :height 140
                    :weight 'normal
                    :width 'normal)

(setq doom-themes-treemacs-enable-variable-pitch nil)

(set-frame-parameter (selected-frame) 'alpha '(97 97))
(add-to-list 'default-frame-alist '(alpha 97 97))
