;;; bookmarks.el --- Bookmarks in Emacs -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defconst *visible-bookmarks-data-file* (path-join *user-data-directory*
                                                   "visible-bookmarks"))
(defconst *bookmark+-data-file* (path-join *user-data-directory* "bookmarks"))
(defconst *bookmark+-menu-state-file* (path-join *user-cache-directory*
                                                 "bookmark-menu-state.el"))

(defun bookmark--local-directory-bookmarks-to-zsh ()
  "Store Emacs bookmarks in ZSH bookmarks file."
  (interactive)
  (when (and (require 'tramp nil t)
             (require 'bookmark nil t))
    (set-buffer (find-file-noselect "~/.zsh_bookmarks" t t))
    (delete-region (point-min) (point-max))
    (insert "# -*- mode:sh -*-\n")
    (let (collect-names)
      (mapc (lambda (item)
              (let ((name (replace-regexp-in-string "-" "_" (car item)))
                    (file (cdr (assoc 'filename
                                      (if (cddr item) item (cadr item))))))
                (when (and (not (tramp-tramp-file-p file))
                           (file-directory-p file))
                  (setq collect-names (cons (concat "~" name) collect-names))
                  (insert (format "%s=\"%s\"\n" name (expand-file-name file))))))
            bookmark-alist)
      (insert ": " (mapconcat 'identity collect-names " ") "\n"))
    (let ((backup-inhibited t)) (save-buffer))
    (kill-buffer (current-buffer))))


(use-package bm
  :defer
  :init
  (user/bind-key-global :code :bookmark-toggle 'bm-toggle)
  (user/bind-key-global :code :bookmark-next 'bm-next)
  (user/bind-key-global :code :bookmark-prev 'bm-previous)
  :config
  (validate-setq
   ;; Persistent bookmarks.
   bm-repository-file *visible-bookmarks-data-file*
   bm-buffer-persistence t)

  ;; Restore bookmarks on file find.
  (add-hook 'find-file-hooks 'bm-buffer-restore)
  ;; Save bookmarks when killing buffer.
  (add-hook 'kill-buffer-hook 'bm-buffer-save)
  ;; Save all bookmarks on exit.
  (add-hook 'kill-emacs-hook 'bm-save)
  ;; Update repository when saving file.
  (add-hook 'after-save-hook 'bm-buffer-save)
  ;; Restore bookmarks when buffer is reverted.
  (add-hook 'after-revert-hook 'bm-buffer-restore))

(use-package bookmark
  :defer
  :init
  ;; Bind bookmarks to C-c b
  (global-set-key (user/get-key :code :bookmark-prefix) 'bookmark-map)
  :config
  (validate-setq
   ;; Enable versioned backups.
   bookmark-version-control t
   bookmark-save-flag 1
   ;; Put the repository in the data directory.
   bookmark-default-file *bookmark+-data-file*)

  ;; Share Emacs directory bookmarks with ZSH.
  (defadvice bookmark-write-file
      (after local-directory-bookmarks-to-zsh-advice activate)
    (bookmark--local-directory-bookmarks-to-zsh))

  (use-package bookmark+
    :defer
    :config
    (validate-setq
     ;; Save bookmarks after ten updates.
     bmkp-count-multi-mods-as-one-flag t)

    ;;; (Bindings) ;;;
    (define-key bookmark-map (kbd "l") 'bookmark-jump)
    (define-key bookmark-map (kbd "e") 'bmkp-edit-bookmark-record)
    (define-key bookmark-map (kbd "t") 'bmkp-add-tags)

    (use-package bookmark+-bmu
      :ensure bookmark+
      :config
      (validate-setq
       ;; Put the menu state in the cache directory.
       bmkp-bmenu-state-file *bookmark+-menu-state-file*))))


(provide 'utilities/bookmarks)
;;; bookmarks.el ends here
