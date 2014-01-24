;;; google-this.el --- Google item under point
;;; Commentary:
;;; Code:

(defun user/google-this-init ()
  "Initialize Google this."
  (google-this-mode t)
  (after-load 'diminish
    (diminish 'google-this-mode))

  ;;; (Bindings) ;;;
  (define-key user/documentation-map (kbd "g RET") 'google-this)
  (define-key user/documentation-map (kbd "g SPC") 'google-region)
  (define-key user/documentation-map (kbd "g s") 'google-search))

(require-package '(:name google-this
                         :type github
                         :pkgname "Bruce-Connor/emacs-google-this"
                         :prepare (progn
                                    (autoload 'google-this-mode "google-this"))
                         :after (user/google-this-init)))


(provide 'utilities/google-this)
;;; google-this.el ends here
