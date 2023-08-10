;;; kubernetes.el --- Configure Emacs kubernetes support -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package kubernetes
  :if (executable-find "kubectl")
  :config
  (use-package timonier)
  (use-package kubedoc))


(provide 'utilities/kubernetes)
;;; kubernetes.el ends here
