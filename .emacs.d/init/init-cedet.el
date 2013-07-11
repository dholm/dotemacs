
;; Set up paths to caches
(setq-default semanticdb-default-save-directory (path-join *user-cache-directory* "semanticdb")
              ede-project-placeholder-cache-file (path-join *user-cache-directory* "ede-projects.el")
              srecode-map-save-file (path-join *user-cache-directory* "srecode-map.el"))


;; Global CEDET initialization
(defun dholm/cedet-init ()
  ;; Set up and enable semantic
  (require 'semantic/ia)
  (require 'semantic/db)

  (semantic-mode)
  (semantic-load-enable-excessive-code-helpers)
  (global-semanticdb-minor-mode)

  ;; Check if GNU Global is available
  (when (cedet-gnu-global-version-check)
    (semanticdb-enable-gnu-global-databases 'c-mode)
    (semanticdb-enable-gnu-global-databases 'c++-mode))

  ;; Check if ectag is available
  (when (cedet-ectag-version-check)
    (semantic-load-enable-primary-ectags-support))

  ;; Enable SRecode templates globally
  (global-srecode-minor-mode)

  ;; Configure ede-mode project management
  (global-ede-mode t)
  (ede-enable-generic-projects))


;; Load standard CEDET features
(dholm/cedet-init)


;; Hooks for setting up cedet when going into a supported mode
(defun dholm/cedet-hook ()
  ;; Set up local bindings
  (local-set-key [(control return)] 'semantic-ia-complete-symbol)
  (local-set-key (kbd "C-c ?") 'semantic-ia-complete-symbol-menu)
  (local-set-key (kbd "C-c >") 'semantic-complete-analyze-inline)
  (local-set-key (kbd "C-c =") 'semantic-decoration-include-visit)
  (local-set-key (kbd "C-c j") 'semantic-ia-fast-jump)
  (local-set-key (kbd "C-c r") 'semantic-symref)
  (local-set-key (kbd "C-c q") 'semantic-ia-show-doc)
  (local-set-key (kbd "C-c s") 'semantic-ia-show-summary)
  (local-set-key (kbd "C-c p") 'semantic-analyze-proto-impl-toggle)
  (local-set-key (kbd "C-c +") 'semantic-tag-folding-show-block)
  (local-set-key (kbd "C-c -") 'semantic-tag-folding-fold-block)
  (local-set-key (kbd "C-c C-c +") 'semantic-tag-folding-show-all)
  (local-set-key (kbd "C-c C-c -") 'semantic-tag-folding-fold-all)

  ;; Use semantic as a source for auto complete
  (set (make-local-variable 'ac-sources)
       (append ac-sources '(ac-source-semantic))))


(defun dholm/c-mode-cedet-hook ()
  (dholm/cedet-hook)
  ;; Load extra semantic helpers
  (require 'semantic/bovine/c)
  (require 'semantic/bovine/gcc)
  (require 'semantic/bovine/clang)

  ;; Local bindings
  (local-set-key (kbd "C-c t") 'eassist-switch-h-cpp)
  (local-set-key (kbd "C-x t") 'eassist-switch-h-cpp)
  (local-set-key (kbd "C-c e") 'eassist-list-methods)
  (local-set-key (kbd "C-c C-r") 'semantic-symref)

  ;; Autocompletion
  (auto-complete-mode t)
  (when (cedet-gnu-global-version-check t)
    (set (make-local-variable 'ac-sources)
         (append ac-sources '(ac-source-gtags)))))


(defun dholm/python-mode-cedet-hook ()
  (dholm/cedet-hook)
  (require 'semantic/wisent/python))


(defun dholm/javascript-mode-cedet-hook ()
  (dholm/cedet-hook)
  (require 'semantic/wisent/javascript))
