;;; init-emacs.el --- initializes basic Emacs settings -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defconst *user-custom-file* (path-join *user-data-directory* "custom.el"))

;; Improve init performance.
(setq
 ;; Increase garbage collection threshold.
 gc-cons-threshold (* 128 1024 1024)
 ;; Don't invoke any file name handlers.
 file-name-handler-alist nil)

;; Restore garbage collection threshold while Emacs is idle.
(run-with-idle-timer
 2 nil
 (lambda ()
   (validate-setq
    ;; Reduce number of pauses due to garbage collection.
    gc-cons-threshold (* 50 1024 1024)
    gc-cons-percentage 0.5
    ;; Restore original file name handlers.
    file-name-handler-alist *user--file-name-handler-alist-original*)))


;; Create data and cache directories
(make-directory *user-cache-directory* t)
(make-directory *user-data-directory* t)

(setq
 ;; Lines of history in the message buffer.
 message-log-max 10000
 ;; Path to custom-file
 custom-file *user-custom-file*)


(provide 'init-emacs)
;;; init-emacs.el ends here
