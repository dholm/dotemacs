;;; org.el --- Org mode support
;;; Commentary:
;;; Code:

(defconst *user-org-data-directory*
  (path-join *user-documents-directory* "Org")
  "Path to user's org data store.")

(defconst *user-org-cache-directory*
  (path-join *user-cache-directory* "org")
  "Path to user's org cache store.")

(defvar user/org-mobile-sync-timer nil
  "Timer used for syncing OrgMobile.")

(defvar user/org-mobile-sync-secs (* 60 20)
  "Interval of OrgMobile sync in seconds.")


(defun user/org-mode-hook ()
  "Org mode hook."
  (unless (derived-mode-p 'text-mode)
    (user/text-mode-hook))

  (setq
   ;; Proper filling of org-mode text, form:
   ;;  * http://lists.gnu.org/archive/html/emacs-orgmode/2008-01/msg00375.html
   paragraph-separate
   "\f\\|\\*+ \\|[ ]*$\\| [ \t]*[:|]\\|^[ \t]+\\[[0-9]\\{4\\}-"
   paragraph-start
   (concat "\f\\|[ ]*$\\|\\*+ \\|\f\\|[ \t]*\\([-+*][ \t]+\\|"
           "[0-9]+[.)][ \t] +\\)\\|[ \t]*[:|]\\|"
           "^[ \t]+\\[[0-9]\\{4\\}-"))

  (user/smartparens-enable)

  (with-feature 'guide-key
    ;; org-mode specific key guides.
    (guide-key/add-local-guide-key-sequence "C-c")
    (guide-key/add-local-guide-key-sequence "C-c C-x")
    (guide-key/add-local-highlight-command-regexp "org-"))

  ;;; (Bindings) ;;;
  (user/bind-key-local :basic :open-buffer-context 'org-iswitchb)
  (user/bind-key-local :basic :narrow-to-page 'org-narrow-to-subtree)
  (user/bind-key-local :basic :narrow-to-region 'org-narrow-to-block)
  (user/bind-key-local :basic :narrow-to-function 'org-narrow-to-element)
  (user/bind-key-local :code :context-promote 'org-shiftup)
  (user/bind-key-local :code :context-demote 'org-shiftdown))


(defun user/org-agenda-finalize-hook ()
  "Org agenda display hook."
  ;; Enable appointment notifications.
  (org-agenda-to-appt t))


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
  (let ((agenda-data-store (path-join *user-org-data-directory* "agendas")))
    (setq-default
     ;; Agenda data store.
     org-agenda-files `(,agenda-data-store)
     ;; Ignore agenda files that are unavailable.
     org-agenda-skip-unavailable-files t
     ;; Restore window configuration when done with the agenda.
     org-agenda-restore-windows-after-quit t
     ;; Start on Monday.
     org-agenda-start-on-weekday 1
     ;; Show month by default.
     org-agenda-span 'month
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
     ;; Include Emacs' Diary in org-agenda.
     org-agenda-include-diary t
     ;; Switch window when opening org-agenda.
     org-agenda-window-setup 'other-window
     ;; Display indirect buffers in the "current" window.
     org-indirect-buffer-display 'current-window)

    ;; Ensure that agenda data store exists.
    (make-directory agenda-data-store t))

  (after-load 'org
    ;; Load org agenda.
    (add-to-list 'org-modules 'org-agenda))

  (when (not noninteractive)
    ;; When running in batch, don't setup windows.
    (setq-default
     ;; Show agenda in current window.
     org-agenda-window-setup 'current-window))

  ;;; (Hooks) ;;;
  (add-hook 'org-agenda-finalize-hook 'user/org-agenda-finalize-hook)

  ;;; (Bindings) ;;;
  (user/bind-key-global :apps :agenda 'org-agenda)
  (user/bind-key-global :apps :todo 'org-todo-list))


(defun user/org-open-at-point (&optional arg)
  "Like `org-open-at-point' but will use external browser with prefix ARG."
  (interactive "P")
  (if (not arg)
      (org-open-at-point)
      (let ((browse-url-browser-function #'browse-url-default-browser))
        (org-open-at-point))))


(defun user/org-annotate-file-storage-file ()
  "Get the path to the annotation storage file."
  (or (with-project-root project-root (path-abs-buffer)
        (path-join project-root (concat (user/proj-name project) ".org")))
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
  (after-load 'org
    (add-to-list 'org-modules 'org))

  (autoload 'org-annotate-file "org-annotate-file" nil t)

  ;;; (Bindings) ;;;
  (user/bind-key-global :util :annotate-buffer 'user/org-annotate-file))


(defun user/org-mobile-sync-pull-and-push ()
  "Sync OrgMobile directory."
  (org-mobile-pull)
  (org-mobile-push)
  (after-load 'sauron
    (sauron-add-event 'my 3 "Called org-mobile-pull and org-mobile-push")))


(defun user/org-mobile-sync-start ()
  "Start automated `org-mobile-push'."
  (interactive)
  (setq user/org-mobile-sync-timer
        (run-with-idle-timer user/org-mobile-sync-secs t
                             'user/org-mobile-sync-pull-and-push)))


(defun user/org-mobile-sync-stop ()
  "Stop automated `org-mobile-push'."
  (interactive)
  (cancel-timer user/org-mobile-sync-timer))


(defun user/org-mobile-init ()
  "Initialize org mobile."
  (setq-default
   ;; Location of TODO items to sync.
   org-mobile-inbox-for-pull org-default-notes-file
   ;; MobileOrg sync directory.
   org-mobile-directory (path-join *user-org-data-directory* "mobile")
   ;; Custom agenda view.
   org-mobile-force-id-on-agenda-items nil))


(defun user/org-babel-init ()
  "Initialize org babel."
  (setq-default
   ;; Don't ask for validation.
   org-confirm-babel-evaluate nil)

  ;; Org babel modules to load by default.
  (after-load 'org
    (add-many-to-list 'org-babel-load-languages
                      ;; Emacs Lisp support.
                      '(emacs-lisp . t)
                      ;; Shell script support.
                      '(shell . t))

    (with-executable 'ditaa
      (add-to-list 'org-babel-load-languages '(ditaa . t)))
    (with-executable 'dot
      (add-to-list 'org-babel-load-languages '(dot . t)))
    (with-executable 'ghc
      (add-to-list 'org-babel-load-languages '(haskell . t)))
    (with-executable 'gnuplot
      (add-to-list 'org-babel-load-languages '(gnuplot . t)))
    (with-executable 'latex
      (add-to-list 'org-babel-load-languages '(latex . t)))
    (with-executable 'perl
      (add-to-list 'org-babel-load-languages '(perl . t)))
    (when (feature-p 'plantuml-mode)
      (setq-default
       org-plantuml-jar-path (path-join (el-get-package-directory 'plantuml-mode)
                                        "plantuml.jar"))
      (add-to-list 'org-babel-load-languages '(plantuml . t)))
    (with-executable 'python
      (add-to-list 'org-babel-load-languages '(python . t)))
    (with-executable 'R
      (add-to-list 'org-babel-load-languages '(R . t)))
    (with-executable 'ruby
      (add-to-list 'org-babel-load-languages '(ruby . t)))))


(defun user/org-export-init ()
  "Initialize org export."
  (setq-default
   ;; Export as UTF-8.
   org-export-coding-system 'utf-8)

  (after-load 'org
    ;; Org export modules to load by default.
    (add-many-to-list 'org-export-backends
                      ;; Ascii support.
                      'ascii
                      ;; HTML.
                      'html
                      ;; OpenDocument Text support.
                      'odt)

    (with-executable 'latex
      (add-many-to-list 'org-export-backends
                        ;; Beamer presentation export.
                        'beamer
                        ;; Plain LaTeX export.
                        'latex))))


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
   ;; Clock data store.
   org-clock-persist-file (path-join *user-org-cache-directory*
                                     "org-clock-save.el")
   ;; Pressing return on a link follows it.
   org-return-follows-link t
   ;; Log time for TODO state changes.
   org-log-done 'time
   ;; Log time when rescheduling an entry.
   org-log-reschedule 'time
   org-log-redeadline 'time
   ;; Round clock times to 15 minute increments.
   org-time-stamp-rounding-minutes (quote (1 15))
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

  (when (eq default-terminal-coding-system 'utf-8)
    (setq-default
     ;; Prettify content using UTF-8.
     org-pretty-entities t))

  (setq-default
   ;; State transitions (http://doc.norang.ca/org-mode.html).
   org-todo-keywords
   (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
           (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE"
                     "MEETING")))
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
           ("j" "Journal" entry
            (file+datetree (path-join *user-org-data-directory* "diary.org"))
            "* %?\n%U\n" :clock-in t :clock-resume t)
           ("w" "org-protocol" entry (file org-default-notes-file)
            "* TODO Review %c\n%U\n" :immediate-finish t)
           ("m" "Meeting" entry (file org-default-notes-file)
            "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
           ("p" "Phone call" entry (file "~/git/org/refile.org")
            "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
           ("h" "Habit" entry (file org-default-notes-file)
            (concat "* NEXT %?\n%U\n%a\n"
                    "SCHEDULED: "
                    "%(format-time-string \"<%Y-%m-%d %a .+1d/3d>\")\n:"
                    "PROPERTIES:\n:STYLE: habit\n"
                    ":REPEAT_TO_STATE: NEXT\n:END:\n")))))

  (after-load 'org
    (add-many-to-list 'org-modules
                      ;; File attachment manager.
                      'org-attach
                      ;; Link to BibTeX entries.
                      'org-bibtex
                      ;; Link to tags.
                      'org-ctags
                      ;; Link to articles and messages in Gnus.
                      'org-gnus
                      ;; Habit tracking.
                      'org-habit
                      ;; Support links to info pages.
                      'org-info
                      ;; Support links to man pages.
                      'org-man
                      ;; Export org buffer to MIME email message.
                      'org-mime
                      ;; Allow external applications to talk to org.
                      'org-protocol
                      ;; Embed source code in org-mode.
                      'org-src)

    (when (feature-p 'bbdb)
      (add-to-list 'org-modules 'org-bbdb))
    (when (feature-p 'emacs-w3m)
      (add-to-list 'org-modules 'org-w3m))
    (when (feature-p 'wanderlust)
      (add-to-list 'org-modules 'org-wl))
    (with-executable 'git
      (add-to-list 'org-modules 'org-git-link)))

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
  (user/org-mobile-init)

  (after-load 'smartparens
    (defun sp--org-skip-asterisk (ms mb me)
      (or (and (= (line-beginning-position) mb)
               (eq 32 (char-after (1+ mb))))
          (and (= (1+ (line-beginning-position)) me)
               (eq 32 (char-after me)))))

    (sp-with-modes 'org-mode
      (sp-local-pair "*" "*" :actions '(insert wrap)
                     :unless '(sp-point-after-word-p sp-point-at-bol-p)
                     :wrap "C-*" :skip-match 'sp--org-skip-asterisk)
      (sp-local-pair "/" "/" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
      (sp-local-pair "~" "~" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
      (sp-local-pair "=" "=" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
      (sp-local-pair "«" "»")))

  ;;; (Hooks) ;;;
  (add-hook 'org-load-hook 'user/org-load-hook)
  (add-hook 'org-mode-hook 'user/org-mode-hook)

  ;;; (Bindings) ;;;
  (user/bind-key-global :apps :capture-task 'org-capture)
  (after-load 'org
    (define-key org-mode-map (kbd "C-c C-o") #'user/org-open-at-point)))


(defun user/org-sync-init ()
  "Initialize org sync."
  (setq-default
   ;; Org sync cache store.
   os-cache-file (path-join *user-org-cache-directory* "org-sync-cache"))

  (after-load 'os
    ;; Redmine module.
    (load "os-rmine")
    ;; GitHub module.
    (load "os-github")))


(defun user/org-init ()
  "Initialize org mode."
  ;; Create data and cache stores.
  (make-directory *user-org-data-directory* t)
  (make-directory *user-org-cache-directory* t)

  ;; Fix for EIN if org hasn't been setup yet.
  (autoload 'org-add-link-type "org" "" t)

  ;;; (Packages) ;;;
  (use-package org
    :ensure t
    :config (user/org-mode-init))
  (use-package org-sync
    :defer t
    :config (user/org-sync-init))
  (use-package ox-mediawiki
    :defer t)
  (use-package org-caldav
    :defer t))

(user/org-init)


(provide 'modes/org)
;;; org.el ends here
