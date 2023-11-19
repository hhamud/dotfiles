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
;;(use-package dashboard
  ;;:ensure t
 ; :config
 ; (dashboard-setup-startup-hook))

;;(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

;; doom themes
(setq doom-theme 'doom-spacegrey)
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
           ((tags "-projects/+NEXT|+INPROGRESS"
                  ((org-agenda-prefix-format "  %i %-12:c [%e] ")
                   (org-agenda-overriding-header "\nTasks\n")))

            (tags "projects/NEXT"
                   ((org-agenda-prefix-format "  %i %-12:c [%e] ")
                        (org-agenda-overriding-header "\n Projects Tasks\n")))

            (agenda ""
                    ((org-agenda-entry-types '(:deadline))
                     (org-deadline-warning-days 7)
                     (org-agenda-skip-function
                      '(org-agenda-skip-entry-if 'notregexp "\\* TODO"))
                     (org-agenda-overriding-header "\nDeadlines\n")))

            (tags-todo "+daily"
                       ((org-agenda-prefix-format "  %i %-12:c [%e] ")
                        (org-agenda-overriding-header "\nDaily Goals\n")))

            (tags-todo "+weekly"
                       ((org-agenda-prefix-format "  %i %-12:c [%e] ")
                        (org-agenda-overriding-header "\nWeekly Goals\n")))

            (tags-todo "+monthly"
                       ((org-agenda-prefix-format "  %i %-12:c [%e] ")
                        (org-agenda-overriding-header "\nMonthly Tasks\n")))

            (tags-todo "+yearly"
                       ((org-agenda-prefix-format "  %i %-12:c [%e] ")
                        (org-agenda-overriding-header "\nAnnual Goals\n")))

            (tags "CLOSED>=\"<today>\""
                  ((org-agenda-overriding-header "\nCompleted Today\n")))))

          ("R" "Monthly Review"
           ((tags "CLOSED>=\"<-1m>\""
                  ((org-agenda-overriding-header "\nCompleted This Month\n")
                   (org-agenda-prefix-format " %-12:c%?-12t% s")
                   ))))
          ))

(setq org-columns-default-formatc
      "%25ITEM %TODO %3PRIORITY %CLOSED")

(setq
    org-superstar-headline-bullets-list '("⁖" "◉" "○" "✸" "✿"))

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


(defun org-roam-backlink ()
     "display the backlinks of the current org-roam buffer"
     (interactive)
     (progn
        (display-buffer (get-buffer-create org-roam-buffer))
        (org-roam-buffer-persistent-redisplay)))

;;(add-hook 'md-roam-mode-hook 'org-roam-backlink)

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

;;(use-package! websocket
    ;;:after org-roam)

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
  (setq md-roam-file-extension-single "md")
  (setq org-roam-file-extensions '("org" "md"))
  (setq md-roam-file-extension "md") ; default "md". Specify an extension such as "markdown"
  (md-roam-mode 1)) ; md-roam-mode must be active before org-roam-db-sync

(org-roam-db-autosync-mode 1) ; autosync-mode triggers db-sync. md-roam-mode must be already active

(defun markdown-string-block()
  "Creates yaml template for md-roam"
  (interactive)
  (insert (format "---\ntitle: \nid: %s\n---" (shell-command-to-string "uuidgen"))))


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

;; pareedit remaps
(map!
 :map paredit-mode-map
 :leader (:prefix ("l" . "Lisps")
         :nvie "f" #'paredit-forward-slurp-sexp
         :nvie "b" #'paredit-forward-barf-sexp
         :nie "k" #'paredit-kill-region
         :nie "s" #'paredit-splice-sexp
         :nie "(" #'paredit-wrap-round
         :nie "[" #'paredit-wrap-square
         :nie "{" #'paredit-wrap-curly))

;; increase font size depending on screen size
(if (>= (x-display-pixel-width) 3840)
    (set-face-attribute 'default nil :height 180)
    (set-face-attribute 'default nil :height 120))

;; markdown-dnd-images -> allows dragging and dropping of images into markdown
(require 'markdown-dnd-images)
(setq dnd-save-directory "~/Documents/org/emacs_images")
(setq dnd-save-buffer-name nil)
(setq dnd-view-inline t)


(setq org-table-eval-formulas t)
(global-set-key (kbd "C-c i") 'org-edit-src-code)


(defun copy-todo-to-file (todo-text)
  "Copy a completed todo to a file of your choosing."
  (interactive "sTodo text: ")
  (let* ((dir "~/Documents/org/review/2023/")
         (file (expand-file-name (read-file-name "Copy todo to file: " dir dir))))
    (find-file file)  ;; Open the file
    (goto-char (point-min))  ;; Go to the beginning of the file
    (if (re-search-forward "What did I accomplish" nil t)
        ;; If the "What did I accomplish" heading is found
        (progn
          (forward-line)  ;; Go to the next line
          (insert (format "CLOSED: [%s] %s\n" (format-time-string "%Y-%m-%d %H:%M") todo-text)))  ;; Insert the closed date and todo text
      (error "Heading not found"))
    (save-buffer)  ;; Save the file
    (kill-buffer)))  ;; Close the file


(defun copy-region-todo-to-file (start end)
  "Copy the content of the selected region as a todo to a file of your choosing."
  (interactive "r")
  (let ((todo-text (buffer-substring-no-properties start end)))
    (copy-todo-to-file todo-text)))


(add-hook 'org-after-todo-state-change-hook
          (lambda ()
            (when (string= org-state "DONE")
              (copy-todo-to-file (org-get-heading t t)))))

;; set default frame size upon open for emacs
(add-to-list 'default-frame-alist '(height . 100))
(add-to-list 'default-frame-alist '(width . 100))

;;;; frame keybindings
(defun search-new-frame (workspace)
  "Creates a new frame after searching for a file."
  (interactive "Fselect file:")
  (let ((new_buffer (find-file-noselect workspace)))
  (make-frame)
  (set-window-buffer (selected-window) new_buffer)))


(defun search-project ()
  "Creates a new frame from the projects directory."
  (interactive)
  (let ((file (read-file-name "Select file: " "~/Documents/projects/")))
    (find-file-other-frame file)
    ))

(defun new-terminal ()
  "Create a new frame with a vterm buffer."
  (interactive)
  (let ((vterm-buffer
    (make-frame `((name . "vterm")
                  (buffer . ,(vterm))))))
    ))

(global-set-key (kbd "C-c f") 'search-new-frame)
(global-set-key (kbd "C-c d") 'delete-frame)
(global-set-key (kbd "C-c t") 'new-terminal)
(global-set-key (kbd "C-c n") 'search-project)

(defun cider-jackin ()
  "Create a new CIDER REPL frame."
  (interactive)
  (let ((cider-buffer
         (make-frame `((name . "cider")
                       (buffer . ,(cider))))))
    (cider-switch-to-repl-buffer)
    (delete-other-windows)
    (set-window-buffer (selected-window) cider-buffer)))

(defun org-review-calender-template ()
"Creates a monthly review org template."
(interactive)
(insert (format "* What are my goals?:\n\n\n* What did I accomplish?:\n\n\n* What did I fail to accomplish?:\n\n\n* Why did I fail?:")))



(defun my-auto-insert-dollar ()
  "Automatically insert a dollar sign after inserting a dollar sign."
  (when (and (eq major-mode 'markdown-mode)
             (eq (char-before) ?$))
    (insert "$")
    (backward-char)))

(add-hook 'post-self-insert-hook 'my-auto-insert-dollar)

(defun todo-creator (goal)
  "Creates a todo list"
  (interactive "sWhat is the goal: ")
  (let*  ((terms '("daily" "weekly" "monthly"))
          (terra (completing-read "Choose a time period: " terms nil t))
          (todo (format "** TODO %s :%s:" goal terra)))
       (insert todo)))

(use-package treesit
  :commands (treesit-install-language-grammar nf/treesit-install-all-languages)
  :init
  (setq treesit-language-source-alist
   '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
     (noir "https://github.com/hhamud/tree-sitter-noir")
     (zig . ("https://github.com/GrayJack/tree-sitter-zig"))))
  :config
  (defun nf/treesit-install-all-languages ()
    "Install all languages specified by `treesit-language-source-alist'."
    (interactive)
    (let ((languages (mapcar 'car treesit-language-source-alist)))
      (dolist (lang languages)
	      (treesit-install-language-grammar lang)
	      (message "`%s' parser was installed." lang)
	      (sit-for 0.75)))))

(defun create-blog (title)
  "Creates the yaml entry for the blog post."
  (interactive "sTitle: ")
  (insert (format "---\ntitle: %s\ndate: %s\ndraft: true\ntags: []\n---" title (format-time-string "%Y-%m-%d"))))

(setq doom-user-dir "/Users/user/.dotfiles/.doom.d/")

(setq lsp-rust-server 'rust-analyzer)

