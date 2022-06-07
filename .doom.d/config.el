;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPGmbers, set this to `relative'.
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

;; custom function to create an org file in ~/org/projects.
  (defun hamza/create-notes-file ()
    (interactive)
    (let ((name (read-string "Filename: ")))
      (expand-file-name (format "%s.org"
                                  name) "~/org/projects/")))

(after! org
(setq org-capture-templates '(
  ("t" "todo" entry (file+headline "~/org/agenda.org" "Tasks:")
    "** TODO %?\n   DEADLINE: <%<%Y-%m-%d %a>>\n"
    :empty-lines 1)
  ("p" "Project" entry
   (file hamza/create-notes-file)
    "* #+TITLE: %? \n #+AUTHOR: \n  #+CREATED: <%<%Y-%m-%d %a>>\n   DEADLINE: <%<%Y-%m-%d %a>>\n "
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
(setq org-roam-file-extensions '("org" "md"))
(md-roam-mode 1) ; md-roam-mode must be active before org-roam-db-sync
(setq md-roam-file-extension "md") ; default "md". Specify an extension such as "markdown"
(org-roam-db-autosync-mode 1) ; autosync-mode triggers db-sync. md-roam-mode must be already active


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
(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))


(use-package! md-roam ; load immediately, before org-roam
  :config
  (setq md-roam-file-extension-single "md"))
    ;you can omit this if md, which is the default.


(defun bms/org-roam-rg-search ()
  "Search org-roam directory using consult-ripgrep. With live-preview."
  (interactive)
  (let ((consult-ripgrep-command "rg --null --ignore-case --type org --line-buffered --color=always --max-columns=500 --no-heading --line-number . -e ARG OPTS"))
    (consult-ripgrep org-roam-directory)))
(global-set-key (kbd "C-c rr") 'bms/org-roam-rg-search)


