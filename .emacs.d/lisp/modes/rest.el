;;; rest --- reStructuredText support
;;; Commentary:
;;; Code:

(defun user/rst-mode-hook ()
  "Hook for reStructuredText mode."
  ;; Hook on save
  (add-hook 'write-contents-functions
            '(lambda ()
               ;; Update TOC
               (rst-toc-update)
               ;; Clean up adornments
               (rst-straighten-adornments)))

  ;; Enable auto completion
  (after-load 'auto-complete
    (auto-complete-rst-init)
    (auto-complete-mode t))

  ;;; (Bindings) ;;;
  ;; Lists
  (user/bind-key-local :code :itemize 'rst-bullet-list-region)
  (user/bind-key-local :code :enumerate 'rst-enumerate-region)

  ;; Table of Contents
  (user/bind-key-local :nav :functions/toc 'rst-toc)

  ;; Compilation
  (user/bind-key-local :code :compile 'rst-compile))


(defun user/rst-mode-init ()
  "Initialize reStructuredText mode."
  ;; Register auto modes
  (add-auto-mode 'rst-mode "\\.rst$" "\\.rest$")

  ;; Update TOC automatically if section headers are adjusted
  (add-hook 'rst-adjust-hook 'rst-toc-update)

  ;; Register mode hook
  (add-hook 'rst-mode-hook 'user/rst-mode-hook))

(require-package '(:name rst-mode :after (user/rst-mode-init)))
(require-package '(:name auto-complete-rst
                         :prepare (progn
                                    (autoload 'auto-complete-rst-init "auto-complete-rst"))))


(provide 'modes/rest)
;;; rest.el ends here
