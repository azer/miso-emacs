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

**Dependencies:**

For faster LSP-mode, make sure [LSP booster](https://github.com/blahgeek/emacs-lsp-booster) is available as executable.

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

Editing:

| Key | Command |
| --- | ------- |
| <kbd>C-z</kbd>  | Undo |
| <kbd>C-shift-z</kbd>  | Redo |

Navigation:

| Key | Command |
| --- | ------- |
| <kbd>S-Up</kbd>  | Jump N lines up |
| <kbd>S-Down</kbd>  | Jump N lines down |
| <kbd>M-Left</kbd>  | Switch to left window |
| <kbd>M-Right</kbd>  | Switch to right window |
| <kbd>M-Down</kbd> | Switch to window below (or minibuffer) |
| <kbd>M-Up</kbd> | Switch to window above |
| <kbd>M-o</kbd>  | Switch windows w/ ace-window |
| <kbd>M-s</kbd>  | Search buffer w/ swiper |

Programming:

| Key | Command |
| --- | ------- |
| <kbd>M-i e</kbd> | List errors (LSP) |
| <kbd>M-i d</kbd> | Toggle debugging mode |
| <kbd>M-i r</kbd> | Rename (LSP) |
| <kbd>M-i a</kbd> | Execute code action suggested by LSP |
| <kbd>M-i j</kbd> | Jump to a symbol in the buffer |
| <kbd>M-i i</kbd> | Re-organize imports |
| <kbd>M-i g</kbd> | Go-to-imlementation |
| <kbd>M-i t</kbd> | Go to type definition |
| <kbd>M-i d</kbd> | Find references |
| <kbd>M-i f</kbd> | Format buffer |
| <kbd>M-i l</kbd> | Go to last change |
| <kbd>M-i -</kbd> | Revert buffer |
| <kbd>M-\</kbd> | Yasnippet |
| <kbd>M-RET</kbd> | Trigger + Complete Copilot |
| <kbd>M-y</kbd> | Trigger + Complete Copilot |
| <kbd>M-\</kbd> | Yasnippet |
| <kbd>M-\</kbd> | Yasnippet menu |
| <kbd>M-i y</kbd> | Yasnippet menu |

Copilot:

| Key | Command |
| --- | ------- |
| <kbd>M-y</kbd> | Complete or accept |
| <kbd>C-Ret</kbd> | Accept by line |
| <kbd>M-Ret</kbd> | Accept by word |

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
| <kbd>M-g c</kbd> | Create commit |
| <kbd>M-g d</kbd> | Git diff for working tree |
| <kbd>M-g f</kbd> | Git diff for the active buffer |
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
