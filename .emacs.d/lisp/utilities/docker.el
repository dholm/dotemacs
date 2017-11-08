;;; docker.el --- Configure Emacs docker support -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package docker
  :if (executable-find "docker")
  :diminish docker-mode
  :config
  (use-package docker-tramp)

  ;; Enabled docker support globally.
  (docker-global-mode t))


(provide 'utilities/docker)
;;; docker.el ends here
