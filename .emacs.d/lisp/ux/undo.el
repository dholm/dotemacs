;;; undo.el --- Configure Emacs undo
;;; Commentary:
;;; Code:

(defconst *user-undo-tree-cache-directory*
  (path-join *user-cache-directory* "undo-tree")
  "Path to user's undo-tree cache store.")


(defun user--undo-tree-config ()
  "Initialize undo-tree."
  ;; Ensure that cache store exists.
  (make-directory *user-undo-tree-cache-directory* t)

  (setq-default
   ;; Set up undo history cache store.
   undo-tree-history-directory-alist
   `((".*" . ,*user-undo-tree-cache-directory*))
   ;; Persistent undo history.
   undo-tree-auto-save-history t
   ;; Don't display in mode-line.
   undo-tree-mode-lighter nil
   ;; Display time stamps in visualizer by default.
   undo-tree-visualizer-timestamps t
   ;; Display diffs in visualizer by default.
   undo-tree-visualizer-diff t)

  ;; Enable globally.
  (global-undo-tree-mode t)

  ;; Compress undo history.
  (defadvice undo-tree-make-history-save-file-name
    (after undo-tree activate)
    (setq ad-return-value (concat ad-return-value ".gz")))

  ;;; (Bindings) ;;;
  (user/bind-key-global :basic :undo 'undo-tree-undo)
  (user/bind-key-global :basic :redo 'undo-tree-redo)
  (user/bind-key-global :util :undo-tree 'undo-tree-visualize))


(defun user--undo-config ()
  "Initialize Emacs undo."
  ;;; (Bindings) ;;;
  (user/bind-key-global :basic :undo 'undo)

  ;;; (Packages) ;;;
  (use-package undo-tree
    :ensure t
    :bind* (([remap undo] . undo-tree-undo)
            ([remap redo] . undo-tree-redo))
    :config (user--undo-tree-config)))

(user--undo-config)


(provide 'ux/undo)
;;; undo.el ends here
