;;; docker.el --- Configure Emacs Docker mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package docker
  :if (executable-find "docker")
  :config
  (use-package docker-tramp)

  ;; Enabled docker support globally.
  (docker-global-mode t))

(use-package dockerfile-mode)
(use-package docker-compose-mode)


(provide 'modes/docker)
;;; docker.el ends here
