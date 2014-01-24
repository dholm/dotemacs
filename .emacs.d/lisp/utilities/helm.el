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
     ((derived-mode-p 'go-mode) (helm-other-buffer
                                 '(helm-source-go-package) buffer-name))
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
  (let ((helm-sources '(helm-source-buffers-list
                        helm-source-fixme))
        (current-file (or (buffer-file-name) default-directory)))
    (when (user/project-p current-file)
      (add-to-list 'helm-sources 'helm-source-bookmarks)
      (if (user/ede-project current-file)
          (add-to-list 'helm-sources 'helm-source-semantic)
        (when (user/gnu-global-tags-p current-file)
          (add-to-list 'helm-sources 'helm-source-gtags-select))))
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
    ;; Fall back to helm-mini if an error occurs in one of the sources
    (error (helm-mini))))


(defun user/helm-init ()
  "Initialize helm."
  ;; Enable helm mode
  (helm-mode t)
  (after-load 'diminish
    (diminish 'helm-mode))

  (setq-default
   ;; Idle delays
   helm-idle-delay 0.1
   helm-input-idle-delay 0.0
   ;; Limit the number of candidates to a reasonable amount
   helm-candidate-number-limit 2000
   ;; Delay showing results that are off screen
   helm-quick-update t
   ;; Put adaptive history in cache directory.
   helm-adaptive-history-file (path-join *user-cache-directory* "helm-adaptive-history"))

  ;; Filter out boring buffers
  (dolist (pattern
           (list "\\*clang-complete" "\\*CEDET global" "\\*tramp/scpc"
                 "\\*epc con" "\\*Pymacs" "\\*Completions\\*"))
    (add-to-list 'helm-boring-buffer-regexp-list pattern))

  ;; Filter out boring files
  (dolist (pattern
           (list "\\.elc$" "\\.pyc$" "^#.+#$" "^G[R]TAGS$" "^GPATH$" "^ID$"))
    (add-to-list 'helm-boring-file-regexp-list pattern))

  ;; Load sources
  (require 'helm-misc)
  (require 'helm-semantic)

  ;;; (Bindings) ;;;
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-x b") 'helm-buffers-list)

  (define-key user/navigation-map (kbd "SPC") 'user/helm-navigate)
  (define-key user/documentation-map (kbd "SPC") 'user/helm-apropos))


(defun user/helm-descbinds-init ()
  "Initialize helm-descbinds."
  (require 'helm-descbinds)
  (helm-descbinds-mode t))


(defun user/helm-gtags-mode-hook ()
  "Mode hook for helm-gtags."
  ;; Automatically update GNU Global database if it exists
  (when (user/gnu-global-tags-p (buffer-file-name))
    (setq-default
     helm-gtags-auto-update t
     helm-gtags-tag-location (user/gnu-global-tags-location (buffer-file-name)))))


(defun user/helm-gtags-init ()
  "Initialize helm-gtags."
  (setq-default
   ;; Don't care about case when searching tags
   helm-c-gtags-ignore-case t
   ;; Tags are read only
   helm-c-gtags-read-only t)

  (add-hook 'helm-gtags-mode-hook 'user/helm-gtags-mode-hook))


(defun user/helm-cmd-t-init ()
  "Initialize Helm cmd-t."
  ;;; (Bindings) ;;;
  (define-key user/navigation-map (kbd "t") 'helm-cmd-t))


(require-package '(:name helm :after (user/helm-init)))
(require-package '(:name helm-descbinds :after (user/helm-descbinds-init)))
(require-package '(:name helm-etags-plus))
(require-package '(:name helm-build-command))
(require-package '(:name helm-cmd-t
                         :type github
                         :pkgname "lewang/helm-cmd-t"
                         :depends (helm)
                         :after (user/helm-cmd-t-init)))

(when *has-git*
  (require-package '(:name helm-ls-git)))
(when *has-global*
  (require-package '(:name helm-gtags
                           :type github
                           :pkgname "syohex/emacs-helm-gtags"
                           :depends (helm)
                           :after (user/helm-gtags-init))))
(when *has-python*
  (require-package '(:name helm-pydoc
                           :type github
                           :depends (helm)
                           :pkgname "syohex/emacs-helm-pydoc")))
(when *has-go*
  (require-package '(:name helm-go-package
                           :type github
                           :pkgname "yasuyk/helm-go-package"
                           :depends (helm go-mode))))


(provide 'utilities/helm)
;;; helm.el ends here
