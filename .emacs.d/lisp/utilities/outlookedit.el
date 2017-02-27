;;; outlookedit.el --- outlook integration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(when (eq system-type 'windows-nt)
  (use-package outlookedit
    :defer
    :quelpa (outlookedit
             :fetcher github
             :repo "dholm/outlookedit")))


(provide 'utilities/outlookedit)
;;; outlookedit.el ends here
