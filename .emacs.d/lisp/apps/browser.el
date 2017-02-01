;;; browser.el --- Web browsing.
;;; Commentary:
;;; Code:

(defvar user/browser-alist
  '((:w3m . ((:launch . w3m)
             (:browse-url . w3m-browse-url)))
    (:eww . ((:launch . eww)
             (:browse-url . eww-browse-url)))
    (:default . ((:launch . nil)
                 (:browse-url . user/browse-url-external)))))


(defun user/browse-url-external ()
  "Browse url using external browser."
  (interactive)
  (let ((browse-url-browser-function 'browse-url-default-browser))
    (call-interactively 'browse-url)))


(defun user--browser-config ()
  "Initialize web browsing in Emacs."
  (let ((browser
         (cond ((feature-p 'eww) (assq :eww user/browser-alist))
               ((feature-p 'emacs-w3m) (assq :w3m user/browser-alist))
               (t (assq :default user/browser-alist))))
        (external-browser (assq :default user/browser-alist)))
    (validate-setq
     browse-url-browser-function (cdr (assq :browse-url browser)))

    ;;; (Bindings) ;;;
    (user/bind-key-global :apps :browse (cdr (assq :launch browser)))
    (user/bind-key-global :apps :browse-external
                          (cdr (assq :browse-url external-browser)))
    (user/bind-key-global :nav :open 'browse-url-at-point)))

(user--browser-config)


(provide 'apps/browser)
;;; browser.el ends here
