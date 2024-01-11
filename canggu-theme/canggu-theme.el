;;; canggu-theme.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Author: Azer Koculu <https://github.com/azer>
;; Created: September 5, 2021
;; Version: 1.0.0
;; Keywords: custom themes, faces
;; Homepage: https://github.com/azer/canggu-theme
;; Package-Requires: ((emacs "25.1") (cl-lib "0.5") (doom-themes "2.2.1"))
;;
;; commentary
;;
;; forked doom-one and customized colors of azer/jungle-theme
;;
;;; Code:

(require 'doom-themes)

;;
;;; Variables

(defgroup canggu-theme nil
  "Options for the `canggu' theme."
  :group 'doom-themes)

(defcustom canggu-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'canggu-theme
  :type 'boolean)

(defcustom canggu-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'canggu-theme
  :type 'boolean)

(defcustom canggu-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'canggu-theme
  :type '(choice integer boolean))


;;
;;; Theme definition

(def-doom-theme canggu
  "A dark theme"

  ;; name        default   256           16
  ((bg         '("#050505" "black"       "black"  ))
   (fg         '("#f8f8f2" "#f8f8f2"     "brightwhite"  ))

   ;; These are off-color variants of bg/fg, used primarily for `solaire-mode',
   ;; but can also be useful as a basis for subtle highlights (e.g. for hl-line
   ;; or region), especially when paired with the `doom-darken', `doom-lighten',
   ;; and `doom-blend' helper functions.
   (bg-alt     '("#010101" "black"       "black"        ))
   (fg-alt     '("#eeeeee" "#eeeeee"     "white"        ))

   ;; These should represent a spectrum from bg to fg, where base0 is a starker
   ;; bg and base8 is a starker fg. For example, if bg is light grey and fg is
   ;; dark grey, base0 should be white and base8 should be black.
   (base0      '("#1B2229" "black"       "black"        ))
   (base1      '("#1c1f24" "#1e1e1e"     "brightblack"  ))
   (base2      '("#202328" "#2e2e2e"     "brightblack"  ))
   (base3      '("#23272e" "#262626"     "brightblack"  ))
   (base4      '("#3f444a" "#3f3f3f"     "brightblack"  ))
   (base5      '("#5B6268" "#525252"     "brightblack"  ))
   (base6      '("#73797e" "#6b6b6b"     "brightblack"  ))
   (base7      '("#9ca0a4" "#979797"     "brightblack"  ))
   (base8      '("#f8f8f2" "#f8f8f2"     "white"        ))

   (grey       base4)
   (red        '("#F2786D" "#F2786D" "red"          ))
   (orange     '("#FFA14F" "#FFA14F" "brightred"    ))
   (melon      '("#FFbb88" "#FFbb88" "yellow"    ))
   (dark-melon '("#cc9955" "#cc9955" "yellow"    ))
   (green      '("#66D977" "#66D977" "green"        ))
   (teal       '("#A6E22E" "#A6E22E" "brightgreen"  ))
   (yellow     '("#D9C27E" "#D9C27E" "yellow"       ))
   (blue       '("#66D9EF" "#66D9EF" "brightblue"   ))
   (dark-blue  '("#4AA3BD" "#4AA3BD" "blue"         ))
   (magenta    '("#FE7AB2" "#FE7AB2" "brightmagenta"))
   (violet     '("#B488F2" "#B488F2" "magenta"      ))
   (light-cyan '("#ACF2E4" "#ACF2E4" "brightcyan"))
   (cyan       '("#66D9EF" "#66D9EF" "brightcyan"   ))
   (dark-cyan  '("#77C3B4" "#77C3B4" "cyan"         ))

   ;; These are the "universal syntax classes" that doom-themes establishes.
   ;; These *must* be included in every doom themes, or your theme will throw an
   ;; error, as they are used in the base theme defined in doom-themes-base.
   (highlight      blue)
   (vertical-bar   (doom-darken base1 0.1))
   (selection      dark-blue)
   (builtin        violet)
   (comments       (if canggu-brighter-comments dark-cyan base5))
   (doc-comments   (doom-lighten (if canggu-brighter-comments dark-cyan base5) 0.25))
   (constants      (doom-lighten dark-melon 0.5))
   (functions      cyan)
   (keywords       cyan)
   (methods        violet)
   (operators      magenta)
   (type           green)
   (strings        red)
   (variables      melon)
   (numbers        yellow)
   (region         (doom-lighten bg 0.1))
   ;;(region         `(,(doom-lighten (car bg-alt) 0.05) ,@(doom-lighten (cdr base1) 0.35)))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; These are extra color variables used only in this theme; i.e. they aren't
   ;; mandatory for derived themes.
   (modeline-fg              fg)
   (modeline-fg-alt          base5)
   (modeline-bg              (if canggu-brighter-modeline
                                 (doom-darken blue 0.45)
                               (doom-darken bg-alt 0.1)))
   (modeline-bg-alt          (if canggu-brighter-modeline
                                 (doom-darken blue 0.475)
                               `(,(doom-darken (car bg-alt) 0.15) ,@(cdr bg))))
   (modeline-bg-inactive     `(,(car bg-alt) ,@(cdr base1)))
   (modeline-bg-inactive-alt `(,(doom-darken (car bg-alt) 0.1) ,@(cdr bg)))

   (-modeline-pad
    (when canggu-padded-modeline
      (if (integerp canggu-padded-modeline) canggu-padded-modeline 4))))


  ;;;; Base theme face overrides
  (((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)
   ((font-lock-comment-face &override)
    :background (if canggu-brighter-comments (doom-lighten bg 0.05)))
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground (if canggu-brighter-modeline base8 highlight))

   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)
   ;;;; doom-modeline
   (doom-modeline-bar :background (if canggu-brighter-modeline modeline-bg highlight))
   (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
   (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-buffer-project-root :foreground green :weight 'bold)
   ;;;; elscreen
   (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")
   ;;;; ivy
   (ivy-current-match :background dark-blue :distant-foreground base0 :weight 'normal)
   ;;;; LaTeX-mode
   (font-latex-math-face :foreground green)
   ;;;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   ((markdown-code-face &override) :background (doom-lighten bg 0.05))
   ;;;; rjsx-mode
   (rjsx-tag :foreground red)
   (rjsx-attr :foreground orange)
   ;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-alt)))

   ;;;; lsp-headerline
   (lsp-headerline-breadcrumb-path-face :foreground grey :background bg)
   (lsp-headerline-breadcrumb-symbols-face :foreground base7 :background bg)
   ;; (lsp-headerline-breadcrumb-path-error-face :inherit lsp-headerline-breadcrumb-path-face)
   ;; (lsp-headerline-breadcrumb-path-hint-face :inherit lsp-headerline-breadcrumb-path-face)
   ;; (lsp-headerline-breadcrumb-path-info-face :inherit lsp-headerline-breadcrumb-path-face)
   ;; (lsp-headerline-breadcrumb-path-warning-face :inherit lsp-headerline-breadcrumb-path-face)
   ;; (lsp-headerline-breadcrumb-symbols-error-face :inherit lsp-headerline-breadcrumb-symbols-face)
   ;; (lsp-headerline-breadcrumb-symbols-hint-face :inherit lsp-headerline-breadcrumb-symbols-face)
   ;; (lsp-headerline-breadcrumb-symbols-info-face :inherit lsp-headerline-breadcrumb-symbols-face)
   ;; (lsp-headerline-breadcrumb-symbols-warning-face :inherit lsp-headerline-breadcrumb-symbols-face)
   (header-line :background bg :foreground base7 :box nil :family "Monaco" :height 0.8 :line-width 4)

   (markdown-header-delimiter-face :foreground base3)
   (markdown-header-face-1 :height 1.0 :foreground light-cyan :weight 'bold)
   (markdown-header-face-2 :foreground cyan)
   (markdown-header-face-3 :foreground dark-cyan)
   (markdown-header-face-4 :foreground yellow)
   (markdown-header-face-5 :foreground melon)
   (markdown-header-face-6 :foreground dark-melon)

   ;; treemacs
   (hl-line-face :background (doom-lighten bg 0.035))
   (hl-line :background (doom-lighten bg 0.05))
   ;;(set-face-background hl-line-face (doom-lighten bg 0.035))
   (treemacs-hl-line-face :background (doom-lighten bg 0.035))

   ;;;; tags
   (font-lock-function-name-face :foreground yellow :weight 'bold)
   (lsp-ui-doc-background          :background base2)
   (button :foreground red)
   )

  ;;;; Base theme variable overrides-
  ())

;;; canggu-theme.el ends here
