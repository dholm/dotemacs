;;; helm.el --- improved Emacs control
;;; Commentary:
;;; Code:

(defun user/helm-apropos ()
  "A context-aware helm apropos."
  (interactive)
  (let ((buffer-name "*helm-apropos*"))
    (cond
     ((derived-mode-p 'emacs-lisp-mode) (helm-apropos))
     ((derived-mode-p 'sh-mode) (helm-other-buffer
                                 '(helm-source-man-pages
                                   helm-source-info-pages) buffer-name))
     ((derived-mode-p 'c-mode-common) (helm-other-buffer
                                       '(helm-source-man-pages) buffer-name))
     ((derived-mode-p 'python-mode) (helm-pydoc))
     (t (message (format "Apropos is unavailable for %S" major-mode))))))


(defun user/helm-navigate ()
  "A context-aware helm navigation aid."
  (interactive)
  (cond
     ((derived-mode-p 'prog-mode) (user/helm-navigate-prog))
     (t (user/helm-navigate-generic))))


(defun user/helm-navigate-prog ()
  "A context-aware helm for programming modes."
  (interactive)
  (let ((helm-sources '(helm-source-buffers-list))
        (current-file (or (buffer-file-name) default-directory)))
    (with-feature 'helm-misc
      ;; FIXMEs.
      (add-to-list 'helm-sources 'helm-source-fixme)
      ;; Emacs lisp.
      (add-to-list 'helm-sources 'helm-source-emacs-source-defun)
      (add-to-list 'helm-sources 'helm-source-emacs-lisp-expectations)
      (add-to-list 'helm-sources 'helm-source-emacs-lisp-toplevels))

    (when (user/project-p current-file)
      ;; Bookmarks.
      (add-to-list 'helm-sources 'helm-source-bookmarks)
      ;; Semantic.
      (with-feature 'helm-semantic
        (if (user/ede-project current-file)
            (add-to-list 'helm-sources 'helm-source-semantic))))

    (helm-other-buffer helm-sources "*helm-navigate-prog*")))


(defun user/helm-navigate-generic ()
  "A somewhat context-aware generic helm."
  (interactive)
  (condition-case nil
      (let ((helm-sources '(helm-source-buffers-list
                            helm-source-recentf
                            helm-source-file-name-history
                            helm-source-file-cache
                            helm-source-buffer-not-found
                            helm-source-man-pages
                            helm-source-info-pages)))
        (cond
         ((eq system-type 'darwin)
          (progn
            (add-to-list 'helm-sources 'helm-source-mac-spotlight)))
         ((eq system-type 'gnu/linux)
          (progn
            (add-to-list 'helm-sources 'helm-source-tracker-search))))
        (helm-other-buffer helm-sources "*helm-navigate-generic*"))
    ;; Fall back to helm-mini if an error occurs in one of the sources.
    (error (helm-mini))))


(defun user/helm-init ()
  "Initialize helm."
  ;; Enable helm mode.
  (helm-mode t)
  (after-load 'diminish
    (diminish 'helm-mode))

  (setq-default
   ;; Idle delays.
   helm-idle-delay 0.1
   helm-input-idle-delay 0.0
   ;; Limit the number of candidates to a reasonable amount.
   helm-candidate-number-limit 2000
   ;; Delay showing results that are off screen.
   helm-quick-update t
   ;; Put adaptive history in cache directory.
   helm-adaptive-history-file (path-join *user-cache-directory* "helm-adaptive-history"))

  ;; Filter out boring buffers.
  (dolist (pattern
           (list "\\*clang-complete" "\\*CEDET global" "\\*tramp/scpc"
                 "\\*epc con" "\\*Pymacs" "\\*Completions\\*"))
    (add-to-list 'helm-boring-buffer-regexp-list pattern))

  ;; Filter out boring files.
  (dolist (pattern
           (list "\\.elc$" "\\.pyc$" "^#.+#$" "^G[R]TAGS$" "^GPATH$" "^ID$"))
    (add-to-list 'helm-boring-file-regexp-list pattern))

  ;;; (Bindings) ;;;
  (global-set-key [remap find-file] 'helm-find-files)
  (global-set-key [remap switch-to-buffer] 'helm-buffers-list)

  (user/bind-key-global :nav :context 'user/helm-navigate)
  (user/bind-key-global :doc :apropos 'user/helm-apropos))


(defun user/helm-descbinds-init ()
  "Initialize helm-descbinds."
  (helm-descbinds-mode t))


(defun user/helm-gtags-mode-hook ()
  "Mode hook for helm-gtags."
  ;; Automatically update GNU Global database if it exists.
  (when (user/gnu-global-tags-p (buffer-file-name))
    (setq-default
     helm-gtags-auto-update t
     helm-gtags-tag-location (user/gnu-global-tags-location (buffer-file-name)))))


(defun user/helm-gtags-init ()
  "Initialize helm-gtags."
  (setq-default
   ;; Don't care about case when searching tags.
   helm-c-gtags-ignore-case t
   ;; Tags are read only.
   helm-c-gtags-read-only t)

  (add-hook 'helm-gtags-mode-hook 'user/helm-gtags-mode-hook))


(defun user/helm-cmd-t-init ()
  "Initialize Helm cmd-t."
  (setq-default
   ;; Use project helpers to find root.
   helm-cmd-t-default-repo '(lambda ()
                              (user/current-path-apply 'user/project-root)))

  ;;; (Bindings) ;;;
  (user/bind-key-global :basic :open-file-context 'helm-cmd-t))


(defun user/helm-swoop-init ()
  "Initialize Helm Swoop."
  (setq-default
   ;; Split window vertically when swooping.
   helm-swoop-split-direction 'split-window-horizontally)

  ;;; (Bindings) ;;;
  (user/bind-key-global :basic :swoop 'helm-swoop)
  (user/bind-key-global :basic :swoop-multi 'helm-swoop-multi)
  (define-key isearch-mode-map
    (user/get-key :basic :swoop) 'helm-swoop-from-isearch)
  (after-load 'helm-swoop
    ;; From helm-swoop to helm-multi-swoop-all.
    (define-key helm-swoop-map
      (user/get-key :basic :swoop) 'helm-multi-swoop-all-from-helm-swoop)))


(require-package '(:name helm :after (user/helm-init)))
(require-package '(:name helm-descbinds :after (user/helm-descbinds-init)))
(require-package '(:name helm-etags-plus))
(require-package '(:name helm-build-command))
(require-package '(:name helm-cmd-t :after (user/helm-cmd-t-init)))
(require-package '(:name helm-swoop :after (user/helm-swoop-init)))

(with-executable 'git
  (require-package '(:name helm-ls-git)))
(with-executable 'global
  (require-package '(:name helm-gtags :after (user/helm-gtags-init))))
(with-executable 'python
  (require-package '(:name helm-pydoc)))


(provide 'utilities/helm)
;;; helm.el ends here
