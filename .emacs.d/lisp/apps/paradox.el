;;; paradox.el --- Set up Paradox -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package paradox
  :defer
  :init
  (user/bind-key-global :apps :packages 'paradox-list-packages)
  :config
  (validate-setq
   ;; Nice spinner.
   paradox-spinner-type 'moon
   ;; Show statistics.
   paradox-display-download-count t
   paradox-display-star-count t
   ;; Don't star automatically.
   paradox-automatically-star nil
   paradox-github-token nil))


(provide 'apps/paradox)
;;; paradox.el ends here
