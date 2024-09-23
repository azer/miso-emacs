(message "Setting up Misomacs")

(setenv "LSP_USE_PLISTS" "true")

;; make sure dependencies are installed
(enable "straight")
(include "dependencies")

(enable "emacs")
(enable "consult")
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
(enable "tabs")
(enable "copilot")
(enable "lsp")

(set-cursor-color "#77B8E9")
(setq cursor-type '(bar . 8))

(setq doom-themes-treemacs-enable-variable-pitch nil)

(set-frame-parameter (selected-frame) 'alpha '(97 97))
(add-to-list 'default-frame-alist '(alpha 97 97))
