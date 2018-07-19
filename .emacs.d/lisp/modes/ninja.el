;;; ninja.el --- Initializes Ninja mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package ninja-mode
  :if (executable-find "ninja")
  :defer)


(provide 'modes/ninja)
;;; ninja.el ends here
