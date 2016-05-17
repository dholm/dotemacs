;;; image.el --- Initializes image mode
;;; Commentary:
;;; Code:

(defun user/image-mode-hook ()
  "Image mode hook."
  ;;; (Bindings) ;;;
  (local-set-key (kbd "M-f") (lambda () (interactive) (image-forward-hscroll 10)))
  (local-set-key (kbd "M-b") (lambda () (interactive) (image-backward-hscroll 10)))
  (local-set-key (kbd "M-p") (lambda () (interactive) (image-previous-line 5)))
  (local-set-key (kbd "M-n") (lambda () (interactive) (image-next-line 5))))


(defun user/image-mode-init ()
  "Initialize image mode."
  ;;; (Hooks) ;;;
  (add-hook 'image-mode-hook 'user/image-mode-hook))

(user/image-mode-init)


(provide 'modes/image)
;;; image.el ends here
