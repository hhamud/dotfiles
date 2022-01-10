;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")
(setq org-default-notes-file "~/org/agenda.org")
;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


;; dashboard configuation
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

;; Set the title
(setq dashboard-banner-logo-title "Welcome to Emacs Dashboard")
;; Set the banner
(setq dashboard-startup-banner "~/Pictures/network.png")
;; Value can be
;; 'official which displays the official emacs logo
;; 'logo which displays an alternative emacs logo
;; 1, 2 or 3 which displays one of the text banners
;; "path/to/your/image.gif", "path/to/your/image.png" or "path/to/your/text.txt" which displays whatever gif/image/text you would prefer
(setq dashboard-image-banner-max-width 300)
;; Content is not centered by default. To center, set
(setq dashboard-center-content t)



(setq dashboard-items '((recents  . 5)
                        (projects . 5)
                        (agenda . 10)
                        ))

;; icons for the dashboard
(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)

(after! org
(setq org-capture-templates '(
  ("t" "todo" entry (file+headline "~/org/agenda.org" "Tasks:")
    "** TODO %?\n   DEADLINE: <%<%Y-%m-%d %a>>\n"
    :empty-lines 1)
  ("p" "Project" entry (file+headline "~/org/agenda.org" "Projects:")
    "** PROJ %?\n :PROPERTIES:\n :CREATED: <%<%Y-%m-%d %a>>\n :END:\n   DEADLINE: <%<%Y-%m-%d %a>>\n "
    :empty-lines 1)))

(setq org-todo-keywords        ; This overwrites the default Doom org-todo-keywords
        '((sequence
           "TODO(t)"           ; A task that is ready to be tackled
           "BLOG(b)"           ; Blog writing assignments
           "PROJ(p)"           ; A project that contains other tasks
           "WAIT(w)"           ; Something is holding up this task
           "PROG(r)"
           "|"                 ; The pipe necessary to separate "active" states and "inactive" states
           "DONE(d)"           ; Task has been completed
           "CANCELLED(c)" )))  ; Task has been cancelled


(setq org-roam-directory "~/org/SlipBox")
;;source code block syntax highlighting
(setq org-src-fontify-natively t
    org-src-tab-acts-natively t
    org-confirm-babel-evaluate nil
    org-edit-src-content-indentation 0)
)


;; doom emacs fonts
(setq doom-font (font-spec :family "Source Code Pro" :size 15)
      doom-variable-pitch-font (font-spec :family "EtBembo" :size 18)
      doom-big-font (font-spec :family "Source Code Pro" :size 24)
      )
(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))
(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

(use-package! mixed-pitch
  :hook (org-mode . variable-pitch-mode)
  :config
  (setq mixed-pitch-set-heigth t)
  ;;(set-face-attribute 'variable-pitch nil :height 1.3)
  )
