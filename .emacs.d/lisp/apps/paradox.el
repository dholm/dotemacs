;;; paradox.el --- Set up Paradox -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user--paradox-menu-visit-homepage (pkg)
  "Visit the homepage of package named PKG using external browser."
  (interactive '(nil))
  (let ((browse-url-browser-function 'browse-url-default-browser))
    (paradox-menu-visit-homepage pkg)))

(use-package paradox
  :defer
  :bind-wrap
  (((:key :apps :packages) . paradox-list-packages)
   :map paradox-menu-mode-map
   ("V" . user--paradox-menu-visit-homepage))
  :config
  (validate-setq
   ;; Nice spinner.
   paradox-spinner-type 'moon
   ;; Show statistics.
   paradox-display-download-count t
   paradox-display-star-count t
   ;; Don't star automatically.
   paradox-automatically-star nil
   paradox-github-token nil
   ;; Execute asynchronously.
   paradox-execute-asynchronously t))


(provide 'apps/paradox)
;;; paradox.el ends here
