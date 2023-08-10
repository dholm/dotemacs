;;; docker.el --- Configure Emacs docker support -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package docker
  :if (executable-find "docker")
  :diminish docker-mode
  :bind-wrap ((:key :util :docker) . docker))


(provide 'utilities/docker)
;;; docker.el ends here
