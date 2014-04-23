;;; org.el --- Org mode support
;;; Commentary:
;;; Code:

(defconst *user-org-data-directory*
  (path-join *user-data-directory* "org")
  "Path to user's org data store.")

(defconst *user-org-cache-directory*
  (path-join *user-cache-directory* "org")
  "Path to user's org cache store.")


(defun user/org-mode-hook ()
  "Org mode hook."
  ;;; (Bindings) ;;;
  (user/bind-key-local :code :context-promote 'org-shiftup)
  (user/bind-key-local :code :context-demote 'org-shiftdown))


(defun user/org-load-hook ()
  "Org-mode loaded hook."
  (when (not noninteractive)
    ;; Resume clocking tasks when Emacs is restarted.
    (org-clock-persistence-insinuate))

  ;; Load modules.
  (when org-modules-loaded
    (org-load-modules-maybe 'force))

  ;; Load Babel languages.
  (org-babel-do-load-languages 'org-babel-load-languages
                               org-babel-load-languages))


(defun user/org-agenda-init ()
  "Initialize org agenda."
  (let ((agenda-data-store (path-join *user-org-data-directory* "agendas"))
        (tmp-diary (make-temp-file (path-join *user-org-cache-directory*
                                              "diary"))))
    (setq-default
     ;; Agenda data store.
     org-agenda-files `(,agenda-data-store)
     ;; Start on Monday.
     org-agenda-start-on-weekday t
     ;; Don't display scheduled todos.
     org-agenda-todo-ignore-scheduled 'future
     ;; Don't show nested todos.
     org-agenda-todo-list-sublevels nil
     ;; Don't dim blocked tasks.
     org-agenda-dim-blocked-tasks nil
     ;; Compact block agenda view.
     org-agenda-compact-blocks t
     ;; Position the habit graph to the right.
     org-habit-graph-column 50
     ;; Temporary diary that org-agenda can import from.
     temp-diary tmp-diary
     diary-file tmp-diary
     org-agenda-include-diary t)

    ;; Ensure that agenda data store exists.
    (make-directory agenda-data-store t)

    ;; Delete temporary diary on exit.
    (add-to-list 'kill-emacs-hook
                 (lambda () (delete-file temp-diary))))

  ;; Load org agenda.
  (add-many-to-list-after-load 'org-modules 'org 'org-agenda)

  (when (not noninteractive)
    ;; When running in batch, don't setup windows.
    (setq-default
     ;; Show agenda in current window.
     org-agenda-window-setup 'current-window))

  ;;; (Bindings) ;;;
  (user/bind-key-global :apps :agenda 'org-agenda))


(defun user/org-annotate-file-storage-file ()
  "Get the path to the annotation storage file."
  (if (user/current-path-apply 'user/project-p)
      (let ((project-root (user/current-path-apply 'user/project-root))
            (project-name (user/current-path-apply 'user/project-name)))
        (path-join project-root (concat project-name ".org")))
    org-annotate-file-storage-file))


(defun user/org-annotate-file ()
  "Annotate the current buffer."
  (interactive)
  (with-feature 'org-annotate-file
    (let ((storage-file (user/org-annotate-file-storage-file))
          (popwin-config '(:position :bottom)))
      (popwin:display-buffer-1 (org-annotate-file-show-section storage-file)
                               :default-config-keywords popwin-config))))


(defun user/org-annotate-file-init ()
  "Initialize org mode file annotation."
  (setq-default
   ;; Add link to current line number.
   org-annotate-file-add-search t)

  ;; Load file annotation support.
  (add-many-to-list-after-load 'org-modules 'org 'org-annotate-file)

  (autoload 'org-annotate-file "org-annotate-file" nil t)

  ;;; (Bindings) ;;;
  (user/bind-key-global :util :annotate-buffer 'user/org-annotate-file))


(defun user/org-babel-init ()
  "Initialize org babel."
  (setq-default
   ;; Don't ask for validation.
   org-confirm-babel-evaluate nil)

  ;; Org babel modules to load by default.
  (add-many-to-list-after-load 'org-babel-load-languages 'org
                               '(;; Emacs Lisp support.
                                 (emacs-lisp . t)
                                 ;; Shell script support.
                                 (sh . t)))

  (with-executable 'ditaa
    (add-many-to-list-after-load 'org-babel-load-languages 'org '(ditaa . t)))
  (with-executable 'dot
    (add-many-to-list-after-load 'org-babel-load-languages 'org '(dot . t)))
  (with-executable 'ghc
    (add-many-to-list-after-load 'org-babel-load-languages 'org '(haskell . t)))
  (with-executable 'gnuplot
    (add-many-to-list-after-load 'org-babel-load-languages 'org '(gnuplot . t)))
  (with-executable 'latex
    (add-many-to-list-after-load 'org-babel-load-languages 'org '(latex . t)))
  (with-executable 'perl
    (add-many-to-list-after-load 'org-babel-load-languages 'org '(perl . t)))
  (when (el-get-package-is-installed 'plantuml-mode)
    (setq-default
     org-plantuml-jar-path (path-join (el-get-package-directory 'plantuml-mode)
                                      "plantuml.jar"))
    (add-many-to-list-after-load 'org-babel-load-languages 'org '(plantuml . t)))
  (with-executable 'python
    (add-many-to-list-after-load 'org-babel-load-languages 'org '(python . t)))
  (with-executable 'R
    (add-many-to-list-after-load 'org-babel-load-languages 'org '(R . t)))
  (with-executable 'ruby
    (add-many-to-list-after-load 'org-babel-load-languages 'org '(ruby . t))))


(defun user/org-export-init ()
  "Initialize org export."
  (setq-default
   ;; Export as UTF-8.
   org-export-coding-system 'utf-8)

  ;; Org export modules to load by default.
  (add-many-to-list-after-load 'org-export-backends 'org
                               '(;; Ascii support.
                                 ascii
                                 ;; HTML.
                                 html
                                 ;; OpenDocument Text support.
                                 odt))

  (with-executable 'latex
    (add-many-to-list-after-load 'org-export-backends 'org
                                 '(;; Beamer presentation export.
                                   beamer
                                   ;; Plain LaTeX export.
                                   latex))))


(defun user/org-mode-init ()
  "Initialize org mode."
  (setq-default
   ;; Org data store.
   org-directory *user-org-data-directory*
   ;; Notes data store.
   org-default-notes-file (path-join *user-org-data-directory* "refile.org")
   ;; Annotations data store.
   org-annotate-file-storage-file (path-join *user-org-data-directory*
                                             "annotations.org")
   ;; Pressing return on a link follows it.
   org-return-follows-link t
   ;; Log time for TODO state changes.
   org-log-done 'time
   ;; Log time when rescheduling an entry.
   org-log-reschedule 'time
   org-log-redeadline 'time
   ;; Log drawer state changes.
   org-log-into-drawer t
   ;; Allow single letter commands at beginning of headlines.
   org-use-speed-commands t
   ;; Fontify code blocks by default.
   org-src-fontify-natively t
   ;; Tab should operate according to local mode.
   org-src-tab-acts-natively t
   ;; Don't preserve source code indentation so it can be adapted to document.
   org-src-preserve-indentation nil
   org-edit-src-content-indentation 0
   ;; Prevent editing of invisible regions.
   org-catch-invisible-edits 'error
   ;; Allow fast state transitions.
   org-use-fast-todo-selection t
   ;; Do not record timestamp when using S-cursor to change state.
   org-treat-S-cursor-todo-selection-as-state-change nil
   ;; Allow refile to create parent tasks, with confirmation.
   org-refile-allow-creating-parent-nodes 'confirm
   ;; Open mailto: links using compose-mail.
   org-link-mailto-program '(compose-mail "%a" "%s")
   ;; Start in folded view.
   org-startup-folded t
   ;; Allow alphabetical lists.
   org-alphabetical-lists t)

  (setq-default
   ;; State transitions (http://doc.norang.ca/org-mode.html).
   org-todo-keywords
   (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
           (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING")))
   ;; Triggered state changes.
   org-todo-state-tags-triggers
   (quote (("CANCELLED" ("CANCELLED" . t))
           ("WAITING" ("WAITING" . t))
           ("HOLD" ("WAITING") ("HOLD" . t))
           (done ("WAITING") ("HOLD"))
           ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
           ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
           ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))))
   ;; Capture templates.
   org-capture-templates
   (quote (("t" "todo" entry (file org-default-notes-file)
            "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
           ("r" "respond" entry (file org-default-notes-file)
            "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n"
            :clock-in t :clock-resume t :immediate-finish t)
           ("n" "note" entry (file org-default-notes-file)
            "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
           ("j" "Journal" entry (file+datetree (path-join *user-org-data-directory* "diary.org"))
            "* %?\n%U\n" :clock-in t :clock-resume t)
           ("w" "org-protocol" entry (file org-default-notes-file)
            "* TODO Review %c\n%U\n" :immediate-finish t)
           ("m" "Meeting" entry (file org-default-notes-file)
            "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
           ("p" "Phone call" entry (file "~/git/org/refile.org")
            "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
           ("h" "Habit" entry (file org-default-notes-file)
            (concat "* NEXT %?\n%U\n%a\n"
                    "SCHEDULED: %(format-time-string \"<%Y-%m-%d %a .+1d/3d>\")\n:"
                    "PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")))))

  (add-many-to-list-after-load 'org-modules 'org
                               '(;; File attachment manager.
                                 org-attach
                                 ;; Link to BibTeX entries.
                                 org-bibtex
                                 ;; Link to tags.
                                 org-ctags
                                 ;; Habit tracking.
                                 org-habit
                                 ;; Support links to info pages.
                                 org-info
                                 ;; Support links to man pages.
                                 org-man
                                 ;; Export org buffer to MIME email message.
                                 org-mime
                                 ;; Allow external applications to talk to org.
                                 org-protocol
                                 ;; Embed source code in org-mode.
                                 org-src))

  (when (el-get-package-is-installed 'bbdb)
    (add-many-to-list-after-load 'org-modules 'org 'org-bbdb))
  (when (el-get-package-is-installed 'emacs-w3m)
    (add-many-to-list-after-load 'org-modules 'org 'org-w3m))
  (when (el-get-package-is-installed 'wanderlust)
    (add-many-to-list-after-load 'org-modules 'org 'org-wl))

  (with-executable 'git
    (add-many-to-list-after-load 'org-modules 'org 'org-git-link))

  (when (not noninteractive)
    ;; When running in batch, don't setup time tracking.
    (setq-default
     ;; Resume clocking task on clock-in if the clock is open.
     org-clock-in-resume t
     ;; Separate drawers for clocking and logs.
     org-drawers '("PROPERTIES" "LOGBOOK")
     ;; Save clock data and state changes and notes in the LOGBOOK drawer.
     org-clock-into-drawer t
     ;; Remove clock line if time is zero.
     org-clock-out-remove-zero-time-clocks t
     ;; Stop clock when entry is marked as DONE.
     org-clock-out-when-done t
     ;; Show the amount of time spent on the current task today.
     org-clock-modeline-total 'today
     ;; Resume clock when reopening Emacs.
     org-clock-persist t
     ;; Enable auto clock resolution for finding open clocks.
     org-clock-auto-clock-resolution 'when-no-clock-is-running
     ;; Include current clocking task in clock reports.
     org-clock-report-include-clocking-task t))

  (when (display-graphic-p)
    (setq-default
     ;; Display inline images when starting up.
     org-startup-with-inline-images t))

  (user/org-babel-init)
  (user/org-export-init)
  (user/org-agenda-init)
  (user/org-annotate-file-init)

  (add-hook 'org-load-hook 'user/org-load-hook)
  (add-hook 'org-mode-hook 'user/org-mode-hook)

  ;;; (Bindings) ;;;
  (user/bind-key-global :apps :capture-task 'org-capture))


(defun user/org-init ()
  "Initialize org mode."
  ;; Create data and cache stores.
  (make-directory *user-org-data-directory* t)
  (make-directory *user-org-cache-directory* t)

  ;; Fix for EIN if org hasn't been setup yet.
  (autoload 'org-add-link-type "org" "" t)

  ;;; (Packages) ;;;
  (require-package '(:name org-mode :after (user/org-mode-init)))
  (require-package '(:name org-caldav))

  (when (el-get-package-is-installed 'org-mode)
    ;; Override org-mode built into Emacs.
    (let ((org-mode-path (path-join (el-get-package-directory 'org-mode) "lisp")))
      (add-to-list 'load-path org-mode-path)
      (load (path-join org-mode-path "org-loaddefs.el")))))

(user/org-init)


(provide 'modes/org)
;;; org.el ends here
