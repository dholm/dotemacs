;;; daemons.el --- Daemon control interface -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package daemons
  :defer
  :bind-wrap
  ((:key :apps :daemons) . daemons))


(provide 'apps/daemons)
;;; daemons.el ends here
