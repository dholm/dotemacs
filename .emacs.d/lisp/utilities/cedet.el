;;; cedet.el --- initializes CEDET
;;; Commentary:
;;; Code:

(defun user/cedet-hook ()
  "Hook for modes with CEDET support."
  (with-feature 'cedet
    ;;; (Semantic) ;;;
    (require 'semantic/ia)

    ;; Scan source code automatically during idle time.
    (global-semantic-idle-scheduler-mode t)
    ;; Highlight the first line of the current tag.
    (global-semantic-highlight-func-mode t)
    ;; Initiate inline completion automatically during idle time.
    (global-semantic-idle-completions-mode t)
    ;; Show breadcrumbs during idle time.
    (setq-default
     semantic-idle-breadcrumbs-format-tag-function 'semantic-format-tag-summarize
     semantic-idle-breadcrumbs-separator " ⊃ "
     semantic-idle-breadcrumbs-header-line-prefix " ≝ ")
    (global-semantic-idle-breadcrumbs-mode t)
    ;; Show summary of tag at point during idle time.
    (global-semantic-idle-summary-mode t)

    ;; Enable [ec]tags support
    (when (and (fboundp 'cedet-ectag-version-check)
               (cedet-ectag-version-check t))
      (semantic-load-enable-primary-ectags-support))

    ;; Enable semantic
    (semantic-mode t)

    ;;; (SemanticDB) ;;;
    (require 'semantic/db)
    (global-semanticdb-minor-mode t)

    (when (cedet-cscope-version-check t)
      ;; Use CScope as a database for SemanticDB
      (when (fboundp 'semanticdb-enable-cscope-databases)
        (semanticdb-enable-cscope-databases))
      ;; Use CScope as a source for EDE
      (setq ede-locate-setup-options
            '(ede-locate-cscope ede-locate-base)))

    ;; Enable GNU Global if available
    (when (and (fboundp 'cedet-gnu-global-version-check)
               (cedet-gnu-global-version-check t))
      (semanticdb-enable-gnu-global-databases 'c-mode)
      (semanticdb-enable-gnu-global-databases 'c++-mode))

    ;;; (Context Menu) ;;;
    (when (display-graphic-p)
      (global-cedet-m3-minor-mode t)
      (local-set-key [mouse-2] 'cedet-m3-menu-kbd))

    ;;; (EDE) ;;;
    (global-ede-mode t)
    (ede-enable-generic-projects)
    (add-hook 'ede-minor-mode-hook 'user/ede-minor-mode-hook)

    ;; Use semantic as a source for auto complete.
    (add-ac-sources 'ac-source-semantic)

    (after-load 'cedet-global
      (when (cedet-gnu-global-version-check t)
        (add-ac-sources 'ac-source-gtags)))

    ;;; (Bindings) ;;;
    (user/bind-key-local :code :update-index 'user/cedet-create/update-all)

    (user/bind-key-local :nav :follow-symbol 'semantic-ia-fast-jump)
    (user/bind-key-local :nav :find-symbol 'semantic-symref-find-tags-by-regexp)
    (user/bind-key-local :nav :jump-spec-impl 'semantic-analyze-proto-impl-toggle)
    (user/bind-key-local :nav :references 'semantic-symref)

    (user/bind-key-local :doc :describe 'semantic-ia-show-doc)

    (with-feature 'eassist
      (user/bind-key-local :nav :functions/toc 'eassist-list-methods))))


(defun user/ede-minor-mode-hook ()
  "Hook for EDE minor mode."
  (when (el-get-package-is-installed 'ede-compdb)
    (require 'ede-compdb)))


(defun user/cedet-before-init ()
  "Setup before loading CEDET."
  (setq-default
   ;; Set up paths to caches
   semanticdb-default-save-directory (path-join *user-cache-directory* "semanticdb")
   ede-project-placeholder-cache-file (path-join *user-cache-directory* "ede-projects.el")
   srecode-map-save-file (path-join *user-cache-directory* "srecode-map.el")))


(defun user/cedet-init ()
  "Initialize CEDET."
  ;; Load the contrib package.
  (unless (featurep 'cedet-contrib-load)
    (load (path-join (el-get-package-directory "cedet") "contrib"
                     "cedet-contrib-load.el")))

  ;; Register missing autoloads
  (autoload 'cedet-cscope-version-check "cedet-cscope")
  (autoload 'cedet-idutils-version-check "cedet-idutils")
  (autoload 'wisent-ruby-default-setup "wisent-ruby")

  ;; Register languages from contrib.
  (after-load 'semantic
    (add-to-list 'semantic-new-buffer-setup-functions '(csharp-mode . wisent-csharp-default-setup))
    (add-to-list 'semantic-new-buffer-setup-functions '(php-mode . wisent-php-default-setup))
    (add-to-list 'semantic-new-buffer-setup-functions '(ruby-mode . wisent-ruby-default-setup)))

  ;;; (Functions) ;;;
  (defun user/ede-get-local-var (fname var)
    "For file FNAME fetch the value of VAR from project."
    (let ((current-project (user/ede-project (user/current-path-apply 'user/project-root))))
      (when current-project
        (let* ((ov (oref current-project local-variables))
               (lst (assoc var ov)))
          (when lst
            (cdr lst))))))

  (defun user/cedet-cscope-create/update ()
    "Create or update CScope database at current project root."
    (interactive)
    (with-executable 'cscope
      (unless (cedet-cscope-version-check t)
        (warn "CScope version is too old!")))
    (let ((proj-root (user/current-path-apply 'user/project-root)))
      (when proj-root
        (cedet-cscope-create/update-database proj-root)
        (message (format "CScope database updated at %S" proj-root)))))

  (defun user/cedet-gnu-global-create/update ()
    "Create or update GNU GLOBAL database at current project root."
    (interactive)
    (with-executable 'global
      (unless (cedet-gnu-global-version-check t)
        (warn "GNU GLOBAL version is too old!")))
    (let ((proj-root (user/current-path-apply 'user/project-root)))
      (when proj-root
        (cedet-gnu-global-create/update-database proj-root)
        (message (format "GNU GLOBAL database updated at %S" proj-root)))))

  (defun user/cedet-gnu-idutils-create/update ()
    "Create or update GNU idutils database at current project root."
    (interactive)
    (with-executable 'idutils
      (unless (cedet-idutils-version-check t)
        (warn "GNU idutils is too old!")))
    (let ((proj-root (user/current-path-apply 'user/project-root)))
      (when proj-root
        (cedet-idutils-create/update-database proj-root)
        (message (format "GNU idutils database updated at %S" proj-root)))))

  (defun user/cedet-create/update-all ()
    "Create or update all databases at current project root."
    (interactive)
    (user/cedet-cscope-create/update)
    (user/cedet-gnu-global-create/update)
    (user/cedet-gnu-idutils-create/update)))

(require-package '(:name cedet
                         :before (user/cedet-before-init)
                         :after (user/cedet-init)))
(require-package '(:name ede-compdb))


(provide 'utilities/cedet)
;;; cedet.el ends here
