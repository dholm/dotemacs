;;; rest --- reStructuredText support
;;; Commentary:
;;; Code:

(defun user--rst-mode-hook ()
  "Hook for reStructuredText mode."
  ;; Hook on save.
  (add-hook 'write-contents-functions
            '(lambda ()
               ;; Update TOC
               (rst-toc-update)
               ;; Clean up adornments
               (rst-straighten-adornments)))

  (when (user/auto-complete-p)
    ;; Enable auto completion.
    (after-load 'auto-complete
      (auto-complete-rst-init)))

  ;;; (Bindings) ;;;
  ;; Lists.
  (user/bind-key-local :code :itemize 'rst-bullet-list-region)
  (user/bind-key-local :code :enumerate 'rst-enumerate-region)

  ;; Table of Contents.
  (user/bind-key-local :nav :functions/toc 'rst-toc)

  ;; Compilation.
  (user/bind-key-local :code :compile 'rst-compile))


(use-package rst
  :defer t
  :quelpa (rst
           :fetcher svn
           :url "http://svn.code.sf.net/p/docutils/code/trunk/docutils/tools/editors/emacs")
  :config
  ;; Register auto modes.
  (add-auto-mode 'rst-mode "\\.rst$" "\\.rest$")

  ;; Update TOC automatically if section headers are adjusted.
  (add-hook 'rst-adjust-hook 'rst-toc-update)

  ;; Register mode hook.
  (add-hook 'rst-mode-hook 'user--rst-mode-hook))

(use-package auto-complete-rst
  :defer t)


(provide 'modes/rest)
;;; rest.el ends here
