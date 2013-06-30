(when (featurep 'cedet)
  (load-file "~/.emacs.d/el-get/cedet/contrib/cedet-contrib-load.el")
  (setq semanticdb-default-save-directory "~/.emacs.cache/semanticdb")

  (require 'cedet-files)
  (require 'cedet-graphviz)
  (require 'cedet-global)
  (require 'cedet-cscope)

  (semantic-mode t)
  (semantic-load-enable-excessive-code-helpers)
  (require 'semantic-tag-folding)
  (require 'semantic/ia)
  (require 'semantic/bovine/c)
  (require 'semantic/bovine/gcc)
  (require 'semantic/bovine/clang)
  (require 'semantic/wisent/javascript)
  (require 'semantic/wisent/python)

  (require 'semantic/db)
  (global-semanticdb-minor-mode t)

  (when (cedet-gnu-global-version-check t)
    (semanticdb-enable-gnu-global-databases 'c-mode)
    (semanticdb-enable-gnu-global-databases 'c++-mode))

  (when (cedet-ectag-version-check t)
    (semantic-load-enable-primary-ctags-support))

  (require 'eassist)

  (global-srecode-minor-mode t)

  (global-ede-mode t)
  (ede-enable-generic-projects)

  (require 'compile)
  (setq compilation-disable-input nil)
  (setq compilation-scroll-output t)
  (setq mode-compile-always-save-buffer-p t)

  (defun dholm/gen-std-compile-string ()
    "Generates compilation string for standard GNU Make project"
    (let* ((current-dir (file-name-directory
			 (or (buffer-file-name (current-buffer)) default-directory)))
	   (prj (ede-current-project current-dir))
	   (root-dir (ede-project-root-directory prj)))
      (concat "cd " root-dir "; nice make -j")))

  (defun dholm/cedet-hook ()
    (local-set-key [(control return)] 'semantic-ia-complete-symbol)
    (local-set-key "\C-c?" 'semantic-ia-complete-symbol-menu)
    (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
    (local-set-key "\C-c=" 'semantic-decoration-include-visit)
    (local-set-key "\C-cj" 'semantic-ia-fast-jump)
    (local-set-key "\C-cq" 'semantic-ia-show-doc)
    (local-set-key "\C-cs" 'semantic-ia-show-summary)
    (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle)
    (local-set-key "\C-c+" 'semantic-tag-folding-show-block)
    (local-set-key "\C-c-" 'semantic-tag-folding-fold-block)
    (local-set-key "\C-c\C-c+" 'semantic-tag-folding-show-all)
    (local-set-key "\C-c\C-c-" 'semantic-tag-folding-fold-all))
  (add-hook 'c-mode-common-hook 'dholm/cedet-hook)
  (add-hook 'lisp-mode-hook 'dholm/cedet-hook)
  (add-hook 'emacs-lisp-mode-hook 'dholm/cedet-hook)

  (defun dholm/c-mode-cedet-hook ()
    (local-set-key "\C-ct" 'eassist-switch-h-cpp)
    (local-set-key "\C-xt" 'eassist-switch-h-cpp)
    (local-set-key "\C-ce" 'eassist-list-methods)
    (local-set-key "\C-c\C-r" 'semantic-symref)
    (add-to-list 'ac-sources 'ac-source-gtags))
  (add-hook 'c-mode-common-hook 'dholm/c-mode-cedet-hook))
