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
  ;;:ensure t
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

;; find the correct monthly file review
(after! org
  (setq org-agenda-custom-commands
        '(("g" "Get Things Done (GTD)"
           ((todo "NEXT|INPROGRESS"
                  ((org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'deadline))
                   (org-agenda-prefix-format "  %i %-12:c [%e] ")
                   (org-agenda-overriding-header "\nTasks\n")))

            (agenda ""
                    ((org-agenda-entry-types '(:deadline))
                     (org-deadline-warning-days 7)
                     (org-agenda-skip-function
                      '(org-agenda-skip-entry-if 'notregexp "\\* TODO"))
                     (org-agenda-overriding-header "\nDeadlines\n")))

            (tags-todo "+monthly"
                       ((org-agenda-prefix-format "  %i %-12:c [%e] ")
                        (org-agenda-overriding-header "\nMonthly Tasks\n")))

            (tags-todo "+yearly"
                       ((org-agenda-prefix-format "  %i %-12:c [%e] ")
                        (org-agenda-overriding-header "\nAnnual Goals\n")))

            (tags "CLOSED>=\"<today>\""
                  ((org-agenda-overriding-header "\nCompleted Today\n")))))))



(setq org-refile-targets (quote ((nil :maxlevel . 10)
                             (org-agenda-files :maxlevel . 10))))

(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes (quote confirm))

(setq org-archive-location "~/Documents/org/projects/archive/%s_archive::")

(setq org-capture-templates '(
  ("t" "todo" entry (file+headline "~/Documents/org/agenda.org" "Tasks:")
    "** TODO %?\n   DEADLINE: <%<%Y-%m-%d %a>>\n"
    :empty-lines 1)
  ))

(setq org-todo-keywords        ; This overwrites the default Doom org-todo-keywords
        '((sequence
           "TODO(t)"           ; A task that is ready to be tackled
           "WAIT(w)"           ; Something is holding up this task
           "NEXT(n)"           ; The next task to do in a project
           "INPROGRESS(p)"     ; A task is in progress
           "|"                 ; The pipe necessary to separate "active" states and "inactive" states
           "DONE(d)"           ; Task has been completed
           "INACTIVE(i)"       ; Projects that have yet to be started
           "CANCELLED(c)" )))  ; Task has been cancelled

;; set closed time on done tasks
(setq org-log-done 'time)

(setq org-agenda-files (directory-files-recursively "~/Documents/org/" "\\.org$"))
(setq org-agenda-archives-mode t)
(setq org-roam-directory "~/Documents/org/SlipBox")
(setq org-roam-file-extensions '("org" "md"))
(require 'md-roam)
(md-roam-mode 1) ; md-roam-mode must be active before org-roam-db-sync
(setq md-roam-file-extension "md") ; default "md". Specify an extension such as "markdown"
(org-roam-db-autosync-mode 1) ; autosync-mode triggers db-sync. md-roam-mode must be already active


;;source code block syntax highlighting
(setq org-src-fontify-natively t
    org-src-tab-acts-natively t
    org-confirm-babel-evaluate nil
    org-edit-src-content-indentation 0)
)


;; doom-themes
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



(defun markdown-string-block()
  "Creates yaml template for md-roam"
  (interactive)
  (insert (format "--- \ntitle: \nid: %s \n---" (shell-command-to-string "uuidgen"))))


(defun insert-file-name (file &optional relativep)
  "Read file name and insert it at point.
With a prefix argument, insert only the non-directory part."
(interactive
 (list (read-file-name "File: " default-directory)))
  (when relativep (setq file  (file-name-nondirectory file)))
  (format "%s" file))



(defun insert-clipboard-image-to-buffer (title)
  "Finds the corresponding image from the file."
  (interactive "stitle of image: ")
  (shell-command "bash ~/Documents/Scripts/pasteImage.sh")
  (insert (concat "![ "(file-relative-name title) "](" (call-interactively 'insert-file-name) ")")))


(define-key markdown-mode-map (kbd "C-c u") 'insert-clipboard-image-to-buffer)
(define-key markdown-mode-map (kbd "C-c mm") 'markdown-string-block)


;; notdeft
(add-to-list 'load-path "~/notdeft")
(add-to-list 'load-path "~/notdeft/extras")
(load "notdeft-example")
(setq notdeft-directories '("~/Documents/org/SlipBox/"))
(setq notdeft-extension "md")
(setq notdeft-secondary-extensions '("md" "org" "txt"))


;; beancount
(add-to-list 'load-path "~/beancount-mode")
(require 'beancount)
(add-to-list 'auto-mode-alist '("\\.beancount\\'" . beancount-mode))
(add-hook 'beancount-mode-hook #'outline-minor-mode)
(define-key beancount-mode-map (kbd "C-c C-n") #'outline-next-visible-heading)
(define-key beancount-mode-map (kbd "C-c C-p") #'outline-previous-visible-heading)


;; lsp-pyright
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred


