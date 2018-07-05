;;; cedet.el --- initializes CEDET -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defconst *user-cedet-ectags-enabled* nil)
(defconst *user-cedet-cscope-enabled* nil)
(defconst *user-cedet-gnu-global-enabled* t)


(defun user--cedet-hook ()
  "Hook for modes with CEDET support."
  ;; Enable EDE.
  (ede-minor-mode t)

  ;; Enable semantic.
  (semantic-mode t))


(defun user/ede-get-local-var (fname var)
  "For file FNAME fetch the value of VAR from project."
  (with-project current-project fname
    (when (user/ede-proj-p current-project)
      (let* ((ov (oref current-project local-variables))
             (lst (assoc var ov)))
        (when lst
          (cdr lst))))))


(defun user/cedet-gnu-idutils-create/update ()
  "Create or update GNU idutils database at current project root."
  (interactive)
  (with-executable 'idutils
    (with-feature 'cedet-idutils
      (unless (cedet-idutils-version-check t)
        (warn "GNU idutils is too old!"))
      (with-project-root proj-root nil
        (cedet-idutils-create/update-database proj-root)
        (message (format "GNU idutils database updated at %S" proj-root))))))


(defun user--semantic-mode-hook ()
  "Semantic mode hook."
  (when (semantic-active-p)
    ;; Scan source code automatically during idle time.
    (semantic-idle-scheduler-mode t)
    ;; Show breadcrumbs during idle time.
    (semantic-idle-breadcrumbs-mode t)
    ;; Show summary of tag at point during idle time.
    (semantic-idle-summary-mode t)

    ;; Highlight the first line of the current tag.
    (semantic-highlight-func-mode t)
    ;; Breadcrumbs should be sticky.
    (semantic-stickyfunc-mode t)

    ;; Use semantic as a source for auto complete.
    (add-ac-sources 'ac-source-semantic)

    (user/tags-try-enable)

    ;;; (Bindings) ;;;
    (user/bind-key-local :nav :jump-spec-impl 'semantic-analyze-proto-impl-toggle)))


(defun ede-object-system-include-path ()
  "Return the system include path for the current buffer."
  (when (and (boundp 'ede-object) ede-object)
    (ede-system-include-path ede-object)))


(defun user--ede-minor-mode-hook ()
  "EDE minor mode hook."
  (cond
   ((user/auto-complete-p)
    (with-feature 'auto-complete-c-headers
      (setq
       ;; Configure include path for auto completion.
       achead:get-include-directories-function
       'ede-object-system-include-path)))
   ((user/company-mode-p)
    (with-feature 'auto-complete-c-headers
      (setq
       ;; Configure include path for auto completion.
       company-c-headers-path-system
       'ede-object-system-include-path)))))


(use-package cedet
  :ensure nil
  :config
  (use-package ede
    :ensure nil
    :defer
    :init
    (autoload 'ede-minor-mode "ede" nil t)
    (add-hook 'ede-minor-mode-hook 'user--ede-minor-mode-hook)
    :config
    (use-package ede/base
      :ensure nil
      :config
      (validate-setq
       ede-project-placeholder-cache-file
       (path-join *user-cache-directory* "ede-projects.el")))

    (use-package ede/locate
      :ensure nil
      :config
      (with-eval-after-load 'cedet-contrib-load
        ;; Enable CScope if available.
        (with-feature 'cedet-cscope
          (when (cedet-cscope-version-check t)
            ;; Use CScope as a source for EDE.
            (setq ede-locate-setup-options
                  '(ede-locate-cscope ede-locate-base)))))))


  (use-package semantic
    :ensure nil
    :defer
    :init
    (add-hook 'semantic-mode-hook 'user--semantic-mode-hook)
    :config
    (when *user-cedet-ectags-enabled*
      ;; Enable [ec]tags support.
      (with-feature 'semantic/ectags/util
        (when (and (fboundp 'cedet-ectag-version-check)
                   (cedet-ectag-version-check t))
          (semantic-load-enable-primary-ectags-support))))

    (when *user-cedet-cscope-enabled*
      ;; Enable CScope support.
      (with-feature 'cedet-cscope
        (when (cedet-cscope-version-check t)
          (with-eval-after-load 'semantic/db
            ;; Use CScope as a database for SemanticDB.
            (when (fboundp 'semanticdb-enable-cscope-databases)
              (semanticdb-enable-cscope-databases))))))

    (when *user-cedet-gnu-global-enabled*
      ;; Enable GNU Global if available.
      (with-feature 'cedet-global
        (when (and (fboundp 'cedet-gnu-global-version-check)
                   (cedet-gnu-global-version-check t))
          ;; Register as SemanticDB source.
          (semanticdb-enable-gnu-global-databases 'c-mode)
          (semanticdb-enable-gnu-global-databases 'c++-mode))))

    ;; Disable semantic over Tramp as SemanticDB's save function keeps freezing
    ;; Emacs.
    (add-to-list 'semantic-inhibit-functions
                 (lambda () (tramp-tramp-file-p (buffer-file-name
                                                 (current-buffer)))))
    ;; Ensure semantic is aware that it supports emacs-lisp.
    (add-to-list 'semantic-new-buffer-setup-functions
                 '(emacs-lisp-mode . semantic-default-elisp-setup))
    (with-feature 'cedet-contrib-load
      ;; Register languages from contrib.
      (add-to-list 'semantic-new-buffer-setup-functions
                   '(csharp-mode . wisent-csharp-default-setup)
                   '(vala-mode . wisent-csharp-default-setup))
      (add-to-list 'semantic-new-buffer-setup-functions
                   '(php-mode . wisent-php-default-setup))
      (add-to-list 'semantic-new-buffer-setup-functions
                   '(ruby-mode . wisent-ruby-default-setup)))

    (use-package semantic/idle
      :ensure nil
      :defer
      :config
      (validate-setq
       ;; Nice looking breadcrumbs.
       semantic-idle-breadcrumbs-format-tag-function 'semantic-format-tag-summarize
       semantic-idle-breadcrumbs-separator " ⊃ "
       semantic-idle-breadcrumbs-header-line-prefix " ≝ "))

    (use-package semantic/db
      :ensure nil
      :after semantic
      :init
      (global-semanticdb-minor-mode t)
      :config
      (use-package semantic/db-file
        :ensure nil
        :defer
        :config
        (validate-setq
         semanticdb-default-save-directory (path-join *user-cache-directory*
                                                      "semanticdb"))))

    (use-package semantic/util-modes
      :ensure nil
      :config
      (use-package stickyfunc-enhance)))

  (use-package srecode/map
    :ensure nil
    :defer
    :config
    (validate-setq
     ;; Set up paths to caches
     srecode-map-save-file (path-join *user-cache-directory* "srecode-map.el"))))


(provide 'utilities/cedet)
;;; cedet.el ends here
