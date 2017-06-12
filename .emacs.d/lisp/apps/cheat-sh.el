;;; cheat-sh.el --- Look up topics on cheat.sh -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package cheat-sh
  :defer
  :init
  (user/bind-key-global :apps :cheat-sh 'cheat-sh))


(provide 'apps/cheat-sh)
;;; cheat-sh.el ends here
