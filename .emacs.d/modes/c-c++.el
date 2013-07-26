;;; c/c++ --- initializes C/C++ modes
;;; Commentary:
;;; Code:

(defconst *has-clang* (executable-find "clang"))


(require-package '(:name c-eldoc
			 :type emacswiki
			 :website "https://raw.github.com/emacsmirror/emacswiki.org/master/c-eldoc.el"
			 :prepare (autoload 'c-turn-on-eldoc-mode "c-eldoc" nil t)))
(when *has-clang*
  (require-package '(:name clang-complete-async
			   :type github
			   :pkgname "Golevka/emacs-clang-complete-async"
			   :build '(("make"))
			   :depends (auto-complete)
			   :features (auto-complete-clang-async)
			   :prepare (setq ac-clang-complete-executable
					  (expand-file-name
					   (concat (el-get-package-directory "clang-complete-async") "clang-complete"))))))


(defun dholm/c-mode-cedet-hook ()
  "C mode CEDET hook."
  (dholm/cedet-hook)
  ;; Load eassist from contrib package
  (unless (featurep 'cedet-contrib-load)
    (load (path-join (el-get-package-directory "cedet") "contrib" "cedet-contrib-load.el")))
  (require 'eassist)

  ;; Load extra semantic helpers
  (require 'semantic/bovine/c)
  (require 'semantic/bovine/gcc)
  (require 'semantic/bovine/clang)

  ;; Enable CScope if available
  (require 'cedet-cscope)
  (when (cedet-cscope-version-check)
    (semanticdb-enable-cscope-databases)
    (setq ede-locate-setup-options
	  '(ede-locate-cscope
	    ede-locate-base)))

  ;; Check if GNU Global is available
  (when (cedet-gnu-global-version-check)
    (semanticdb-enable-gnu-global-databases 'c-mode)
    (semanticdb-enable-gnu-global-databases 'c++-mode))

  ;; Local bindings
  (define-key dholm/navigation-map (kbd "t") 'eassist-switch-h-cpp)
  (define-key dholm/documentation-map (kbd "m") 'eassist-list-methods)

  ;; Autocompletion
  (auto-complete-mode t)
  (when (cedet-gnu-global-version-check t)
    (set (make-local-variable 'ac-sources)
         (append ac-sources '(ac-source-gtags)))))


(defun dholm/c-mode-common-hook ()
  "C mode common hook."
  ;; Set the default C/C++ code styles
  (setq-default
   c-default-style "K&R"
   c++-default-style "Stroustrup")
  ;; Load CEDET
  (dholm/c-mode-cedet-hook)
  ;; Enable eldoc
  (c-turn-on-eldoc-mode)
  ;; Override the indentation level of case labels in the K&R- and
  ;; Stroustrup styles so that they are indented one level beyond
  ;; the switch.
  (c-set-offset 'case-label '+)
  ;; Run spell-checker on strings and comments
  (flyspell-prog-mode)
  ;; Separate camel-case into separate words
  (subword-mode t)
  (add-hook 'before-save-hook
            ;; Delete trailing whitespace on save
            'delete-trailing-whitespace nil t)
  ;; Autocompletion
  (when *has-clang*
    (set (make-local-variable 'ac-sources)
         (append ac-sources '(ac-source-clang-async)))
    (ac-clang-launch-completion-process))
  ;; Enable dtrt-indent to attempt to identify the indentation rules used
  (dtrt-indent-mode t))

(add-hook 'c-mode-common-hook 'dholm/c-mode-common-hook)


(provide 'modes/c-c++)
;;; c-c++.el ends here
