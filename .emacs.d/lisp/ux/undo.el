;;; undo.el --- Configure Emacs undo
;;; Commentary:
;;; Code:

(defconst *user-undo-tree-cache-directory*
  (path-join *user-cache-directory* "undo-tree")
  "Path to user's undo-tree cache store.")


(defun user/undo-tree-init ()
  "Initialize undo-tree."
  ;; Ensure that cache store exists.
  (make-directory *user-undo-tree-cache-directory* t)

  (setq-default
   ;; Set up undo history cache store.
   undo-tree-history-directory-alist
   `((".*" . ,*user-undo-tree-cache-directory*))
   ;; Persistent undo history.
   undo-tree-auto-save-history t)

  (global-undo-tree-mode t)
  (after-load 'diminish
    (diminish 'undo-tree-mode))

  ;; Compress undo history.
  (defadvice undo-tree-make-history-save-file-name
    (after undo-tree activate)
    (setq ad-return-value (concat ad-return-value ".gz")))

  ;;; (Bindings) ;;;
  (global-set-key [remap undo] 'undo-tree-undo)
  (global-set-key [remap redo] 'undo-tree-redo)
  (user/bind-key-global :basic :undo 'undo-tree-undo)
  (user/bind-key-global :basic :redo 'undo-tree-redo)
  (user/bind-key-global :util :undo-tree 'undo-tree-visualize))


(defun user/undo-init ()
  "Initialize Emacs undo."
  ;;; (Bindings) ;;;
  (user/bind-key-global :basic :undo 'undo)

  ;;; (Packages) ;;;
  (require-package '(:name undo-tree :after (user/undo-tree-init))))

(user/undo-init)


(provide 'ux/undo)
;;; undo.el ends here
