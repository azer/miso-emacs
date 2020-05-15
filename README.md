# miso-emacs

Opinionated Emacs distro for minimalists.

Features:

- **Writing-mode first:** Spell checker, dictionary and word counter is available for markdown-modes. ![](https://cldup.com/2zt2RPJFwM.png)
- **M-i:** Most commands are binded to `M-i`. (See below).
- **Rabbit-mode enabled:** Use left/right buttons to undo/redo, up/down to jump.
- **IDE:** LSP is enabled by default. A workflow shortcut for DAP (debugger) is provided. Currently supported languages: Go and TypeScript. ![](https://cldup.com/J-zIC1aNBx.png)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Install](#install)
- [Bindings](#Bindings)
- [Customize](#customize)

<!-- markdown-toc end -->

# Install

```bash
$ git clone git@github.com:azer/miso-emacs.git ~/.miso-emacs
$ ln -s .miso-emacs/.emacs .emacs
```

# Bindings

Navigation:

| Key | Command |
| --- | ------- |
| <kbd>Up</kbd>  | Jump N lines up |
| <kbd>Down</kbd>  | Jump N lines down |
| <kbd>Left</kbd>  | Undo |
| <kbd>Right</kbd>  | Redo |
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
| <kbd>M-i up</kbd> | Push local branch to remote |
| <kbd>M-i down</kbd> | Pull from remote branch |
| <kbd>C-c C-j</kbd> | Jump to definition |

Writing:

| Key | Command |
| --- | ------- |
| <kbd>M-i d</kbd> | Define word at point |
| <kbd>M-i t</kbd> | Generate ToC for Markdown |
| <kbd>M-i j</kbd> | Jump to a title |
| <kbd>M-i w</kbd> | Create and switch to a new scratch buffer |
| <kbd>M-i up</kbd> | Push local branch to remote |
| <kbd>M-i down</kbd> | Pull from remote branch |

Project(ile):

| Key | Command |
| --- | ------- |
| <kbd>M-p f</kbd> | Find file in the project |
| <kbd>M-p p</kbd> | Switch projects |
| <kbd>M-p b</kbd> | Switch to buffer |
| <kbd>M-p s</kbd> | Search project |
| <kbd>M-p r</kbd> | Search & replace project |

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
