;;; (Initialization) ;;;

(eval-after-load "flycheck" '(lambda ()
                               (global-flycheck-mode)
                               (diminish 'flycheck-mode)))
