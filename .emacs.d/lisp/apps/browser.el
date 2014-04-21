;;; browser.el --- Web browsing.
;;; Commentary:
;;; Code:

(defvar user/browser-alist
  '((:w3m . ((:launch . w3m)
             (:browse-url . w3m-browse-url)))
    (:eww . ((:launch . eww)
             (:browse-url . eww-browse-url)))
    (:default . ((:launch . nil)
                 (:browse-url . browse-url-default-browser)))))


(defun user/browser-init ()
  "Initialize web browsing in Emacs."
  (let ((browser
         (cond ((feature-p 'eww) (assq :eww user/browser-alist))
               ((executable-find "w3m") (assq :w3m user/browser-alist))
               (t (assq :default user/browser-alist))))
        (external-browser
         (cond ((eq system-type 'darwin) 'browse-url-default-macosx-browser)
               ((eq system-type 'windows-nt) 'browse-url-default-windows-browser)
               (t 'browse-url-default-browser))))
    (setq-default
     browse-url-browser-function (cdr (assq :browse-url browser)))

    ;;; (Bindings) ;;;
    (user/bind-key-global :apps :browse (cdr (assq :launch browser)))
    (user/bind-key-global :apps :browse-external external-browser)
    (user/bind-key-global :nav :open 'browse-url-at-point)))

(user/browser-init)


(provide 'apps/browser)
;;; browser.el ends here
