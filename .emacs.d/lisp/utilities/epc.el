;;; epc --- RPC stack for Emacs
;;; Commentary:
;;; Code:

(defun user/epc-init ()
  "Initialize EPC."
  ;;; (Faces) ;;;
  (after-load 'solarized-theme
    (solarized-with-values
      (eval
       `(custom-theme-set-faces
         'solarized
         '(epc:face-title ((t (:foreground ,blue :background ,solarized-bg
                                           :weight normal :underline nil)))))))))

(require-package '(:name epc :after (user/epc-init)))


(provide 'utilities/epc)
;;; epc.el ends here
