;;; hideshow.el --- Configure hide show
;;; Commentary:
;;; Code:

(defun user--hs-minor-mode-hook ()
  "Minor mode hook for Hide Show."
  ;;; (Bindings) ;;;
  (local-set-key (kbd "C-c SPC") 'user/hs-toggle-level)
  (local-set-key (kbd "C-c <right>") 'hs-show-block)
  (local-set-key (kbd "C-c <left>")  'hs-hide-block)
  (local-set-key (kbd "C-c <up>")    'hs-hide-all)
  (local-set-key (kbd "C-c <down>")  'hs-show-all))


(defun user/hs-toggle-level ()
  "Toggle hide/show for level at point."
  (interactive)
  (hs-show-block)
  (hs-hide-level 1))


(use-package hideshow
  :init
  (add-hook 'hs-minor-mode-hook 'user--hs-minor-mode-hook))


(provide 'utilities/hideshow)
;;; hideshow.el ends here
