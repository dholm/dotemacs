;;; indentation.el --- Configure Emacs indentation behavior
;;; Commentary:
;;; Code:

(defun user--dtrt-indent-config ()
  "Initialize dtrt indent."
  (validate-setq
   dtrt-indent-verbosity 0
   global-mode-string (delq 'dtrt-indent-mode-line-info global-mode-string)))


(defun user--indentation-config ()
  "Initialize Emacs indentation behavior."
  (use-package dtrt-indent
    :ensure t
    :config (user--dtrt-indent-config))
  (use-package smart-tabs-mode
    :quelpa (smart-tabs-mode
             :fetcher github
             :repo "jcsalomon/smarttabs")))

(user--indentation-config)


(provide 'ux/indentation)
;;; indentation.el ends here
