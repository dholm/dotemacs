;;; paths.el --- Configure system paths in Emacs -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))


(provide 'ux/paths)
;;; paths.el ends here
