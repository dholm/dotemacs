;;; c/c++ --- initializes C/C++ modes
;;; Commentary:
;;; Code:

(defconst *has-clang* (executable-find "clang"))


(defun user/c-mode-common-hook ()
  "C mode common hook."
  ;; Set the default C/C++ code styles
  (setq-default
   c-default-style "K&R"
   c++-default-style "Stroustrup")

  ;; Load CEDET
  (user/c-mode-cedet-hook)

  ;; Enable eldoc
  (c-turn-on-eldoc-mode)

  ;; Override the indentation level of case labels in the K&R- and
  ;; Stroustrup styles so that they are indented one level beyond
  ;; the switch.
  (c-set-offset 'case-label '+)

  ;; Separate camel-case into separate words
  (subword-mode t)

  (when (el-get-package-is-installed 'helm-etags-plus)
    ;; Automatically update tags
    (turn-on-ctags-auto-update-mode))

  ;; Autocompletion
  (when *has-clang*
    (set (make-local-variable 'ac-sources)
         (append ac-sources '(ac-source-clang-async)))
    (ac-clang-launch-completion-process))
  ;; Enable dtrt-indent to attempt to identify the indentation rules used
  (dtrt-indent-mode t))

(add-hook 'c-mode-common-hook 'user/c-mode-common-hook)


(defun user/c-mode-cedet-hook ()
  "C mode CEDET hook."
  (user/cedet-hook)
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
  (define-key user/navigation-map (kbd "t") 'eassist-switch-h-cpp)
  (define-key user/documentation-map (kbd "m") 'eassist-list-methods)

  ;; Autocompletion
  (auto-complete-mode t)
  (when (cedet-gnu-global-version-check t)
    (set (make-local-variable 'ac-sources)
         (append ac-sources '(ac-source-gtags)))))


(require-package '(:name c-eldoc
			 :type github
                         :pkgname "mooz/c-eldoc"
                         :depends (deferred)
			 :prepare (autoload 'c-turn-on-eldoc-mode "c-eldoc" nil t)))
(when *has-clang*
  (require-package '(:name clang-complete-async)))


(provide 'modes/c-c++)
;;; c-c++.el ends here
