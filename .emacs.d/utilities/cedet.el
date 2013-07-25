;;; cedet --- initializes CEDET
;;; Commentary:
;;; Code:

(require-package '(:name cedet :after (dholm/cedet-init)))


(setq-default
 ;; Set up paths to caches
 semanticdb-default-save-directory (path-join *user-cache-directory* "semanticdb")
 ede-project-placeholder-cache-file (path-join *user-cache-directory* "ede-projects.el")
 srecode-map-save-file (path-join *user-cache-directory* "srecode-map.el"))


;; Initialize global modes
(defun dholm/cedet-init ()
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


;; Hooks for setting up cedet when going into a supported mode
(defun dholm/cedet-hook ()
  ;; Set up local bindings
  (local-set-key [(control return)] 'semantic-ia-complete-symbol)
  (local-set-key (kbd "C-c ?") 'semantic-ia-complete-symbol-menu)
  (local-set-key (kbd "C-c >") 'semantic-complete-analyze-inline)
  (local-set-key (kbd "C-c =") 'semantic-decoration-include-visit)
  (local-set-key (kbd "C-c n j") 'semantic-ia-fast-jump)
  (local-set-key (kbd "C-c d r") 'semantic-symref)
  (local-set-key (kbd "C-c d d") 'semantic-ia-show-doc)
  (local-set-key (kbd "C-c d s") 'semantic-ia-show-summary)
  (local-set-key (kbd "C-c p") 'semantic-analyze-proto-impl-toggle)
  (local-set-key (kbd "C-c +") 'semantic-tag-folding-show-block)
  (local-set-key (kbd "C-c -") 'semantic-tag-folding-fold-block)
  (local-set-key (kbd "C-c C-c +") 'semantic-tag-folding-show-all)
  (local-set-key (kbd "C-c C-c -") 'semantic-tag-folding-fold-all)

  ;; Use semantic as a source for auto complete
  (when (featurep 'auto-complete)
    (set (make-local-variable 'ac-sources)
       (append ac-sources '(ac-source-semantic)))))


(provide 'utilities/cedet)
;;; cedet.el ends here
