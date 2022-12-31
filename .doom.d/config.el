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

(setq org-columns-default-format
      "%25ITEM %TODO %3PRIORITY %CLOSED")

(setq
    org-superstar-headline-bullets-list '("⁖" "◉" "○" "✸" "✿")
)

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


(defvar my-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x C-c b") 'chatgpt-run)
    (define-key map (kbd "C-x C-c c") 'chatgpt-input-auth-token)
    map)
  "my-keys-minor-mode keymap.")

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t
  :lighter " my-keys")

(my-keys-minor-mode 1)

(setq org-table-eval-formulas t)
(global-set-key (kbd "C-c i") 'org-edit-src-code)


(defun copy-todo-to-file (todo-text)
  "Copy a completed todo to a file of your choosing."
  (interactive "sTodo text: ")
  (let* ((dir "~/Documents/org/review/2022/")
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

(add-hook 'org-after-todo-state-change-hook
          (lambda ()
            (when (string= org-state "DONE")
              (copy-todo-to-file (org-get-heading t t)))))


