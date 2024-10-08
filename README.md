# miso-emacs

Opinionated Emacs distro for minimalists.

| Writing-mode | Programming-mode |
| --- | --- |
| Centered writing layout, spell correction, dictionary and pronounciation shortcuts in markdown modes | Dark color scheme, LSP mode enabled for programing |
| <img width="1230" alt="Screenshot 2024-05-29 at 10 32 45" src="https://github.com/azer/miso-emacs/assets/13072/2a8ffb4f-bd3c-40aa-be82-2a6e62988fbe"> | <img width="1230" alt="Screenshot 2024-05-29 at 10 32 11" src="https://github.com/azer/miso-emacs/assets/13072/f438d8bc-1d49-48c6-b815-52518546f4b8"> |

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Install](#install)
- [Bindings](#Bindings)
- [Customize](#customize)

<!-- markdown-toc end -->

# Install

Setup miso-emacs:

```bash
$ git clone git@github.com:azer/miso-emacs.git ~/.miso-emacs
$ ln -s .miso-emacs/.emacs .emacs
```

Install icon fonts;

```
M-x all-the-icons-install-fonts
M-x nerd-icons-install-fonts
```

Install ispell:

```
$ brew install ispell
```

# Bindings

Navigation:

| Key | Command |
| --- | ------- |
| <kbd>M-Up</kbd>  | Jump N lines up |
| <kbd>M-Down</kbd>  | Jump N lines down |
| <kbd>M-Left</kbd>  | Undo |
| <kbd>M-Right</kbd>  | Redo |
| <kbd>M-o</kbd>  | Switch windows w/ ace-window |
| <kbd>M-s</kbd>  | Search buffer w/ swiper |

Programming:

| Key | Command |
| --- | ------- |
| <kbd>M-i e</kbd> | List errors (LSP) |
| <kbd>M-i d</kbd> | Toggle debugging mode |
| <kbd>M-i r</kbd> | Rename (LSP) |
| <kbd>M-i f</kbd> | Execute code action suggested by LSP |
| <kbd>M-i j</kbd> | Jump to a symbol in the buffer |
| <kbd>M-i i</kbd> | Re-organize imports |
| <kbd>M-i g</kbd> | Go-to-imlementation |
| <kbd>M-i t</kbd> | Go to type definition |
| <kbd>M-i c</kbd> | Find references |
| <kbd>M-i -</kbd> | Revert buffer |
| <kbd>M-\</kbd> | Yasnippet |
| <kbd>M-RET</kbd> | Trigger + Complete Copilot |
| <kbd>M-y</kbd> | Trigger + Complete Copilot |


Writing:

| Key | Command |
| --- | ------- |
| <kbd>M-i d</kbd> | Define word at point |
| <kbd>M-i t</kbd> | Generate ToC for Markdown |
| <kbd>M-i j</kbd> | Jump to a title |
| <kbd>M-i w</kbd> | Create and switch to a new scratch buffer |
| <kbd>M-i t</kbd> | Google translate at point |
| <kbd>M-i p</kbd> | Pronounce at point |
| <kbd>M-i =</kbd> | Reset sizing |


Project(ile):

| Key | Command |
| --- | ------- |
| <kbd>M-p f</kbd> | Find file in the project |
| <kbd>M-p p</kbd> | Switch projects |
| <kbd>M-p b</kbd> | Switch to buffer |
| <kbd>M-p s</kbd> | Search project |
| <kbd>M-p r</kbd> | Search & replace project |

Git:

| Key | Command |
| --- | ------- |
| <kbd>M-g s</kbd> | Git status |
| <kbd>M-g d</kbd> | Git diff for working tree |
| <kbd>M-g b</kbd> | Git diff for the active buffer |
| <kbd>M-g -</kbd> | Stash the worktree |
| <kbd>M-g =</kbd> | Apply stash |
| <kbd>M-i up</kbd> | Push local branch to remote |
| <kbd>M-i down</kbd> | Pull from remote branch |

Others:

| Key | Command |
| --- | ------- |
| <kbd>M-Backspace</kbd>  | Delete backwards |
| <kbd>C-w</kbd>  | Kill region |
| <kbd>C-r</kbd> | Query & replace regexp in the buffer |
| <kbd>C-M-r</kbd> | Query replace at cursor thing |
| <kbd>M-;</kbd> | Comment/uncomment region |
| <kbd>M-c</kbd> | Capitalize the word |

# Customize

Create a `private` folder under `.miso-emacs` and have an `init.el` file to execute custom Elisp.

# Troubleshooting

* **LSP-mode fails to initialize with JSON parsing errors:**

Clean up all packages and start Emacs with env variable below:

```bash
export LSP_USE_PLISTS=true
```

Source: https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization


