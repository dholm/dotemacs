;;; org.el --- Org mode support
;;; Commentary:
;;; Code:

(defconst *user-org-data-directory*
  (path-join *user-data-directory* "org")
  "Path to user's org data store.")


(defun user/org-mode-hook ()
  "Org mode hook."
  (when org-modules-loaded
    (org-load-modules-maybe 'force))

  ;;; (Bindings) ;;;
  (user/bind-key-local :code :context-promote 'org-shiftup)
  (user/bind-key-local :code :context-demote 'org-shiftdown))


(defun user/org-agenda-init ()
  "Initialize org agenda."
  (let ((agenda-data-store (path-join *user-org-data-directory* "agendas")))
    (setq-default
     ;; Agenda data store.
     org-agenda-files `(,agenda-data-store)
     ;; Start on Monday.
     org-agenda-start-on-weekday t
     ;; Don't display scheduled todos.
     org-agenda-todo-ignore-scheduled 'future
     ;; Don't show nested todos.
     org-agenda-todo-list-sublevels nil)

    ;; Ensure that agenda data store exists.
    (make-directory agenda-data-store t))

  (add-to-list 'org-modules 'org-agenda)

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

  (add-to-list 'org-modules 'org-annotate-file)

  (autoload 'org-annotate-file "org-annotate-file" nil t)

  ;;; (Bindings) ;;;
  (user/bind-key-global :util :annotate-buffer 'user/org-annotate-file))


(defun user/org-mode-init ()
  "Initialize Lua mode."
  (setq-default
   ;; Notes data store.
   org-default-notes-file (path-join *user-org-data-directory* "notes.org")
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
   ;; Prevent editing of invisible regions.
   org-catch-invisible-edits 'error)

  ;; Org modules to load by default.
  (setq org-modules
        (append org-modules
                '(;; File attachment manager.
                  org-attach
                  ;; Link to BibTeX entries.
                  org-bibtex
                  ;; Link to tags.
                  org-ctags
                  ;; Support links to info pages.
                  org-info
                  ;; Support links to man pages.
                  org-man
                  ;; Export org buffer to MIME email message.
                  org-mime
                  ;; Embed source code in org-mode.
                  org-src)))

  ;; Org babel modules to load by default.
  (setq org-modules
        (append org-modules
                '(;; Emacs Lisp support.
                  ob-emacs-lisp)))

  ;; Org export modules to load by default.
  (setq org-modules
        (append org-modules
                '(;; Ascii support.
                  ox-ascii
                  ;; OpenDocument Text support.
                  ox-odt)))

  (when (el-get-package-is-installed 'bbdb)
    (setq org-modules (append org-modules '(org-bbdb))))
  (when (el-get-package-is-installed 'emacs-w3m)
    (setq org-modules (append org-modules '(org-w3m))))
  (when (el-get-package-is-installed 'wanderlust)
    (setq org-modules (append org-modules '(org-wl))))

  (with-executable 'ditaa
    (setq org-modules (append org-modules '(ob-ditaa))))
  (with-executable 'dot
    (setq org-modules (append org-modules '(ob-dot))))
  (with-executable 'git
    (setq org-modules (append org-modules '(org-git-link))))
  (with-executable 'gnuplot
    (setq org-modules (append org-modules '(ob-gnuplot))))
  (with-executable 'latex
    (setq org-modules (append org-modules '(ob-latex ox-beamer))))
  (with-executable 'R
    (setq org-modules (append org-modules '(ob-R))))

  (when (not noninteractive)
    ;; When running in batch, don't setup time tracking.
    (setq-default
     ;; Remove clock line if time is zero.
     org-clock-out-remove-zero-time-clocks t
     ;; Stop clock when entry is marked as DONE.
     org-clock-out-when-done t
     ;; Show the amount of time spent on the current task today.
     org-clock-modeline-total 'today
     ;; Resume clock when reopening Emacs.
     org-clock-persist t)

    (after-load 'org-mode
      ;; Initialize clock persistence.
      (org-clock-persistence-insinuate)))

  (when (display-graphic-p)
    (setq-default
     ;; Display inline images when starting up.
     org-startup-with-inline-images t))

  (user/org-agenda-init)
  (user/org-annotate-file-init)

  (add-hook 'org-mode-hook 'user/org-mode-hook))


(defun user/org-init ()
  "Initialize org mode."
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
