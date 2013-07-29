;;; cedet --- initializes CEDET
;;; Commentary:
;;; Code:

(defun dholm/cedet-hook ()
  "Hook for modes with CEDET support."
  ;; Set up local bindings
  (local-set-key [(control return)] 'semantic-ia-complete-symbol)
  (local-set-key (kbd "C-c ?") 'semantic-ia-complete-symbol-menu)
  (local-set-key (kbd "C-c >") 'semantic-complete-analyze-inline)
  (local-set-key (kbd "C-c =") 'semantic-decoration-include-visit)
  (define-key dholm/navigation-map (kbd "j") 'semantic-ua-fast-jump)
  (define-key dholm/documentation-map (kbd "r") 'semantic-symref)
  (define-key dholm/documentation-map (kbd "d") 'semantic-ia-show-doc)
  (define-key dholm/documentation-map (kbd "s") 'semantic-ia-show-summary)
  (local-set-key (kbd "C-c p") 'semantic-analyze-proto-impl-toggle)
  (local-set-key (kbd "C-c +") 'semantic-tag-folding-show-block)
  (local-set-key (kbd "C-c -") 'semantic-tag-folding-fold-block)
  (local-set-key (kbd "C-c C-c +") 'semantic-tag-folding-show-all)
  (local-set-key (kbd "C-c C-c -") 'semantic-tag-folding-fold-all)

  ;; Use semantic as a source for auto complete
  (when (featurep 'auto-complete)
    (set (make-local-variable 'ac-sources)
       (append ac-sources '(ac-source-semantic)))))


(defun dholm/cedet-init ()
  "Initialize CEDET."
  (setq-default
   ;; Set up paths to caches
   semanticdb-default-save-directory (path-join *user-cache-directory* "semanticdb")
   ede-project-placeholder-cache-file (path-join *user-cache-directory* "ede-projects.el")
   srecode-map-save-file (path-join *user-cache-directory* "srecode-map.el"))

  ;; Set up and enable semantic
  (require 'semantic/ia)
  (require 'semantic/db)

  (semantic-mode)
  (semantic-load-enable-excessive-code-helpers)
  (global-semanticdb-minor-mode)

  ;; Check if GNU Global is available
  (when (cedet-gnu-global-version-check t)
    (semanticdb-enable-gnu-global-databases 'c-mode)
    (semanticdb-enable-gnu-global-databases 'c++-mode))

  ;; Check if ectag is available
  (when (cedet-ectag-version-check t)
    (semantic-load-enable-primary-ectags-support))

  ;; Enable SRecode templates globally
  (global-srecode-minor-mode)

  ;; Configure ede-mode project management
  (global-ede-mode t)
  (ede-enable-generic-projects))


(require-package '(:name cedet :after (dholm/cedet-init)))


(provide 'utilities/cedet)
;;; cedet.el ends here
