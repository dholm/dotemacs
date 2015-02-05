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


(defun user/helm-mode ()
  "Start helm-mode."
  ;; helm-org depends on Flyspell, wait for it to be loaded.
  (after-load 'flyspell
    (helm-mode t)

    (with-feature 'helm-descbinds
      (helm-descbinds-mode t))

    (after-load 'diminish
      (diminish 'helm-mode))

    ;; Filter out boring buffers.
    (dolist (pattern
             (list "\\*clang-complete" "\\*CEDET global" "\\*tramp/scpc"
                   "\\*epc con" "\\*Pymacs" "\\*Completions\\*"))
      (add-to-list 'helm-boring-buffer-regexp-list pattern))

    ;; Filter out boring files.
    (dolist (pattern
             (list "\\.elc$" "\\.pyc$" "^#.+#$" "^G[R]TAGS$" "^GPATH$" "^ID$"))
      (add-to-list 'helm-boring-file-regexp-list pattern))))


(defun user/helm-swoop-init ()
  "Initialize Helm Swoop."
  (setq-default
   ;; Split window vertically when swooping.
   helm-swoop-split-direction 'split-window-horizontally)

  ;;; (Bindings) ;;;
  (user/bind-key-global :basic :swoop 'helm-swoop)
  (user/bind-key-global :basic :swoop-multi 'helm-multi-swoop)
  (define-key isearch-mode-map
    (user/get-key :basic :swoop) 'helm-swoop-from-isearch)
  (after-load 'helm-swoop
    ;; From helm-swoop to helm-multi-swoop-all.
    (define-key helm-swoop-map
      (user/get-key :basic :swoop) 'helm-multi-swoop-all-from-helm-swoop)))


(defun user/helm-init ()
  "Initialize helm."
  (setq-default
   ;; Idle delays.
   helm-idle-delay 0.1
   helm-input-idle-delay 0.0
   ;; Limit the number of candidates per source to a reasonable amount.
   helm-candidate-number-limit 75
   ;; Delay showing results that are off screen.
   helm-quick-update t
   ;; Put adaptive history in cache directory.
   helm-adaptive-history-file (path-join *user-cache-directory* "helm-adaptive-history"))

  (after-load 'popwin
    (add-to-list
     'popwin:special-display-config
     '("helm" :regexp t :height 0.4 :position bottom)))

  ;;; (Hooks) ;;;
  ;; Since Helm depends on `eieio', enable it after package initialization.
  (add-hook 'user/after-init-hook 'user/helm-mode)

  ;;; (Bindings) ;;;
  (global-set-key [remap find-file] 'helm-find-files)
  (global-set-key [remap switch-to-buffer] 'helm-buffers-list)
  (global-set-key [remap execute-extended-command] 'helm-M-x)

  (user/bind-key-global :nav :context 'user/helm-navigate)
  (user/bind-key-global :doc :apropos 'user/helm-apropos)
  (user/bind-key-global :emacs :elisp-search 'helm-info-elisp))


(require-package '(:name helm :after (user/helm-init)))
(require-package '(:name helm-descbinds))
(require-package '(:name helm-build-command))
(require-package '(:name helm-swoop :after (user/helm-swoop-init)))


(provide 'utilities/helm)
;;; helm.el ends here
