;;; rest --- reStructuredText support -*- lexical-binding: t; -*-
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
    (with-eval-after-load 'auto-complete
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
  :defer
  :quelpa (rst
           :fetcher svn
           :url "http://svn.code.sf.net/p/docutils/code/trunk/docutils/tools/editors/emacs")
  :mode ("\.\(rst\|rest\)$" . rst-mode)
  :hook ((rst-adjust-hook . rst-toc-update)
         (rst-mode-hook . user--rst-mode-hook))
  :config
  (use-package auto-complete-rst
    :after (auto-complete)))


(provide 'modes/rest)
;;; rest.el ends here
