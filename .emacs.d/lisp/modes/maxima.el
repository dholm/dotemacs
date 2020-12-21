;;; maxima.el --- Maxima integration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package maxima
  :if (executable-find "maxima")
  :defer t
  :mode ("\\.mac\\'" . maxima-mode)
  :interpreter ("maxima" . maxima-mode)
  :config
  (validate-setq
   org-format-latex-options (plist-put org-format-latex-options :scale 2.0)
   maxima-display-maxima-buffer nil)

  (use-package company-maxima
    :config
    (add-to-list 'company-backends '(company-maxima-symbols company-maxima-libraries))))


(provide 'modes/maxima)
;;; maxima.el ends here
