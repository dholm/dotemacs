;;; cedet.el --- initializes CEDET
;;; Commentary:
;;; Code:

(defvar user/semantic-initialized nil
  "Non-nil if Semantic has been initialized.")

(defvar user/ede-initialized nil
  "Non-nil if EDE has been initialized.")


(defun user/cedet-hook ()
  "Hook for modes with CEDET support."
  ;;; (EDE) ;;;
  ;; Don't use with-feature here in case el-get hasn't initialized CEDET yet.
  (when (featurep 'cedet-devel-load)
    ;; Enable EDE.
    (ede-minor-mode t))

  ;;; (Semantic) ;;;
  ;; Enable semantic.
  (semantic-mode t)

  ;; Scan source code automatically during idle time.
  (global-semantic-idle-scheduler-mode t)
  ;; Highlight the first line of the current tag.
  (global-semantic-highlight-func-mode t)
  ;; Initiate inline completion automatically during idle time.
  (global-semantic-idle-completions-mode t)
  ;; Show breadcrumbs during idle time.
  (global-semantic-idle-breadcrumbs-mode t)
  ;; Breadcrumbs should be sticky.
  (global-semantic-stickyfunc-mode t)
  ;; Show summary of tag at point during idle time.
  (global-semantic-idle-summary-mode t)

  ;; Use semantic as a source for auto complete.
  (add-ac-sources 'ac-source-semantic)

  ;; Use GNU GLOBAL as a source for auto complete.
  (with-feature 'cedet-global
    (when (and (fboundp 'cedet-gnu-global-version-check)
               (cedet-gnu-global-version-check t))
      ;; Register as auto-completion source.
      (add-ac-sources 'ac-source-gtags)))

  ;;; (SemanticDB) ;;;
  (with-feature 'semantic/db
    (global-semanticdb-minor-mode t))

  ;;; (Context Menu) ;;;
  (when (and (featurep 'cedet-devel-load) (display-graphic-p))
    (cedet-m3-minor-mode t))

  ;;; (Bindings) ;;;
  (user/bind-key-local :code :update-index 'user/cedet-create/update-all)

  (when (feature-p 'helm)
    (user/bind-key-local :nav :functions/toc 'helm-semantic-or-imenu))
  (unless (and (boundp 'helm-gtags-mode) helm-gtags-mode)
    (user/bind-key-local :nav :find-symbol 'semantic-symref-find-tags-by-regexp)
    (user/bind-key-local :nav :references 'semantic-symref))
  (user/bind-key-local :nav :jump-spec-impl 'semantic-analyze-proto-impl-toggle)

  (user/bind-key-local :doc :describe 'semantic-ia-show-doc))


(defun user/ede-get-local-var (fname var)
  "For file FNAME fetch the value of VAR from project."
  (with-ede-project current-project fname
    (let* ((ov (oref current-project local-variables))
           (lst (assoc var ov)))
      (when lst
        (cdr lst)))))


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


(defun user/cedet-create/update-all ()
  "Create or update all databases at current project root."
  (interactive)
  (user/cscope-create/update)
  (user/gnu-global-create/update)
  (user/cedet-gnu-idutils-create/update))


(defun user/semantic-init-hook ()
  "Semantic initialization hook."
  (unless user/semantic-initialized
    ;; Enable [ec]tags support.
    (with-feature 'semantic/ectags/util
      (when (and (fboundp 'cedet-ectag-version-check)
                 (cedet-ectag-version-check t))
        (semantic-load-enable-primary-ectags-support)))

    (with-feature 'cedet-cscope
      (when (cedet-cscope-version-check t)
        (after-load 'semantic/db
          ;; Use CScope as a database for SemanticDB.
          (when (fboundp 'semanticdb-enable-cscope-databases)
            (semanticdb-enable-cscope-databases)))))

    ;; Enable GNU Global if available.
    (with-feature 'cedet-global
      (when (and (fboundp 'cedet-gnu-global-version-check)
                 (cedet-gnu-global-version-check t))
        ;; Register as SemanticDB source.
        (semanticdb-enable-gnu-global-databases 'c-mode)
        (semanticdb-enable-gnu-global-databases 'c++-mode)))

    (when (featurep 'cedet-devel-load)
      ;; Register languages from contrib.
      (add-to-list 'semantic-new-buffer-setup-functions
                   '(csharp-mode . wisent-csharp-default-setup))
      (add-to-list 'semantic-new-buffer-setup-functions
                   '(php-mode . wisent-php-default-setup))
      (add-to-list 'semantic-new-buffer-setup-functions
                   '(ruby-mode . wisent-ruby-default-setup)))

    (setq user/semantic-initialized t)))


(defun user/ede-init-hook ()
  "EDE initialization hook."
  (unless user/ede-initialized
    ;; Enable CScope if available.
    (with-feature 'cedet-cscope
      (when (cedet-cscope-version-check t)
        ;; Use CScope as a source for EDE.
        (setq ede-locate-setup-options
              '(ede-locate-cscope ede-locate-base))))

    (ede-enable-generic-projects)

    (setq user/ede-initialized t)))


(defun user/cedet-before-init ()
  "Setup before loading CEDET."
  (setq-default
   ;; Set up paths to caches
   semanticdb-default-save-directory (path-join *user-cache-directory*
                                                "semanticdb")
   ede-project-placeholder-cache-file (path-join *user-cache-directory*
                                                 "ede-projects.el")
   srecode-map-save-file (path-join *user-cache-directory* "srecode-map.el")))


(defun user/cedet-init ()
  "Initialize CEDET."
  (setq-default
   ;; Nice looking breadcrumbs.
   semantic-idle-breadcrumbs-format-tag-function 'semantic-format-tag-summarize
   semantic-idle-breadcrumbs-separator " ⊃ "
   semantic-idle-breadcrumbs-header-line-prefix " ≝ ")

  (after-load 'cedet-devel-load
    ;; Load the contrib package.
    (unless (featurep 'cedet-contrib-load)
      (load (path-join (el-get-package-directory "cedet") "contrib"
                       "cedet-contrib-load.el")))

    ;; Register missing autoloads
    (autoload 'wisent-ruby-default-setup "wisent-ruby"))

  (add-hook 'ede-minor-mode-hook 'user/ede-init-hook)
  (add-hook 'semantic-mode-hook 'user/semantic-init-hook))

(require-package '(:name cedet
                         :before (user/cedet-before-init)
                         :after (user/cedet-init)))


(provide 'utilities/cedet)
;;; cedet.el ends here
