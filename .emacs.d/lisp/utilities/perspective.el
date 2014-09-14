;;; perspective.el --- perspectives in Emacs
;;; Commentary:
;;; Code:

(defun user/perspective-init ()
  "Initialize perspective."
  (setq-default
   ;; Always show perspective in the modeline.
   persp-show-modestring t
   frame-title-format '("Emacs: %b [" (:eval (persp-name persp-curr)) "]"))

  ;; Enable perspective mode.
  (persp-mode t)
  (after-load 'projectile
    ;; Integrate with Projectile.
    (require 'persp-projectile nil :noerror))

  ;;; (Bindings) ;;;
  (user/bind-key-global :util :perspective 'persp-switch))

(require-package '(:name perspective :after (user/perspective-init)))


(provide 'utilities/perspective)
;;; perspective.el ends here
