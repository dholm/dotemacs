;;; browser.el --- Web browsing. -*- lexical-binding: t; -*-
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

    (use-package eww
      :ensure nil
      :config
      (use-package helm-eww
        :bind (:map eww-mode-map
                    ([remap eww-list-histories] . helm-eww-history)
                    ([remap eww-list-bookmarks] . helm-eww-bookmarks)
                    ([remap eww-list-buffers] . helm-eww-buffers)))

      (use-package eww-lnum
        :bind (:map eww-mode-map
                    ("f" . eww-lnum-follow)
                    ("F" . eww-lnum-universal))))

    ;;; (Bindings) ;;;
    (user/bind-key-global :apps :browse (cdr (assq :launch browser)))
    (user/bind-key-global :apps :browse-external
                          (cdr (assq :browse-url external-browser)))
    (user/bind-key-global :nav :open 'browse-url-at-point)))

(user--browser-config)


(provide 'apps/browser)
;;; browser.el ends here
