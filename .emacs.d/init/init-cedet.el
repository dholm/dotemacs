
;; Set up paths to caches
(setq semanticdb-default-save-directory "~/.emacs.cache/semanticdb"
      ede-project-placeholder-cache-file "~/.emacs.cache/ede-projects.el"
      srecode-map-save-file "~/.emacs.cache/srecode-map.el")


;; Set up and enable semantic
(require 'semantic/ia)
(require 'semantic/db)
(require 'semantic/bovine/c)
(require 'semantic/bovine/gcc)
(require 'semantic/bovine/clang)
(require 'semantic/wisent/javascript)
(require 'semantic/wisent/python)

(semantic-mode t)
(semantic-load-enable-excessive-code-helpers)
(global-semanticdb-minor-mode t)


;; Check if GNU Global is available
(when (cedet-gnu-global-version-check t)
  (semanticdb-enable-gnu-global-databases 'c-mode)
  (semanticdb-enable-gnu-global-databases 'c++-mode))


;; Check if ectag is available
(when (cedet-ectag-version-check t)
  (semantic-load-enable-primary-ctags-support))


;; Enable SRecode templates globally
(global-srecode-minor-mode t)


;; Configure ede-mode project management
(global-ede-mode t)
(ede-enable-generic-projects)


;; Hooks for setting up cedet when going into a supported mode
(defun dholm/cedet-hook ()
  (local-set-key [(control return)] 'semantic-ia-complete-symbol)
  (local-set-key "\C-c?" 'semantic-ia-complete-symbol-menu)
  (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
  (local-set-key "\C-c=" 'semantic-decoration-include-visit)
  (local-set-key "\C-cj" 'semantic-ia-fast-jump)
  (local-set-key "\C-cr" 'semantic-symref)
  (local-set-key "\C-cq" 'semantic-ia-show-doc)
  (local-set-key "\C-cs" 'semantic-ia-show-summary)
  (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle)
  (local-set-key "\C-c+" 'semantic-tag-folding-show-block)
  (local-set-key "\C-c-" 'semantic-tag-folding-fold-block)
  (local-set-key "\C-c\C-c+" 'semantic-tag-folding-show-all)
  (local-set-key "\C-c\C-c-" 'semantic-tag-folding-fold-all)
  ;; Use semantic as a source for auto complete
  (set (make-local-variable 'ac-sources)
       (append ac-sources '(ac-source-semantic))))
(add-hook 'c-mode-common-hook 'dholm/cedet-hook)
(add-hook 'lisp-mode-hook 'dholm/cedet-hook)
(add-hook 'emacs-lisp-mode-hook 'dholm/cedet-hook)
(add-hook 'python-mode-hook 'dholm/cedet-hook)


(defun dholm/c-mode-cedet-hook ()
  (local-set-key "\C-ct" 'eassist-switch-h-cpp)
  (local-set-key "\C-xt" 'eassist-switch-h-cpp)
  (local-set-key "\C-ce" 'eassist-list-methods)
  (local-set-key "\C-c\C-r" 'semantic-symref)
  ;; Autocompletion
  (auto-complete-mode t)
  (when (cedet-gnu-global-version-check t)
    (set (make-local-variable 'ac-sources)
         (append ac-sources '(ac-source-gtags)))))
(add-hook 'c-mode-common-hook 'dholm/c-mode-cedet-hook)
