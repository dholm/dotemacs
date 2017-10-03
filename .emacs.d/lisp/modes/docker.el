;;; docker.el --- Configure Emacs Docker mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package docker
  :config
  (use-package docker-tramp)
  (use-package dockerfile-mode)
  (use-package docker-compose-mode)

  ;; Enabled docker support globally.
  (docker-global-mode t))


(provide 'modes/docker)
;;; docker.el ends here
