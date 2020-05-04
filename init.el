(message "Setting up Misomacs")

;; make sure dependencies are installed
(include "dependencies")

(enable "emacs")
(enable "ivy")
(enable "writing-mode")
(enable "markdown-mode")
(enable "rabbit-mode")
(enable "undo-tree")
(enable "trailing-whitespace")
(enable "rename-current-buffer")
(enable "programming")
(enable "go")
(enable "typescript")
(enable "javascript")
(enable "projectile")
(enable "anzu")
(enable "modeline")
(enable "scratch")
(enable "debugging")
(enable "windows")

;; enable jungle theme
(load-local-file "jungle-theme/jungle-theme")
(set-cursor-color "#77B8E9")
(setq cursor-type '(bar . 8))
