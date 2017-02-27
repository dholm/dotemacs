;;; indentation.el --- Configure Emacs indentation behavior -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package dtrt-indent
  :config
  (validate-setq
   dtrt-indent-verbosity 0
   global-mode-string (delq 'dtrt-indent-mode-line-info global-mode-string)))

(use-package smart-tabs-mode
  :quelpa (smart-tabs-mode
           :fetcher github
           :repo "jcsalomon/smarttabs"))


(provide 'ux/indentation)
;;; indentation.el ends here
