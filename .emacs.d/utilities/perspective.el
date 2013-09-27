;;; perspective.el --- perspectives in Emacs
;;; Commentary:
;;; Code:

(defun user/perspective-init ()
  "Initialize perspective."
  (setq-default
   ;; Always show perspective in the modeline.
   persp-show-modestring t
   frame-title-format '("Emacs: %b [" (:eval (persp-name persp-curr)) "]"))

  ;;; (Bindings) ;;;
  (define-key user/utilities-map (kbd "p") 'persp-mode)
  (after-load 'perspective
    (define-key persp-mode-map (kbd "C-c p") perspective-map)
    (define-key persp-mode-map (kbd "C-c p SPC") 'persp-switch-quick)))

(require-package '(:name perspective :after (user/perspective-init)))


(provide 'utilities/perspective)
;;; perspective.el ends here
