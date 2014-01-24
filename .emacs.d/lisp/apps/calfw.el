;;; calfw.el --- calendar for Emacs
;;; Commentary:
;;; Code:

(defun user/calfw-init ()
  "Initialize calfw."
  ;; Weeks start on Monday
  (setq-default calendar-week-start-day 1)

  ;; Use unicode to render calendar
  (setq-default
   cfw:fchar-junction ?╬
   cfw:fchar-vertical-line ?║
   cfw:fchar-horizontal-line ?═
   cfw:fchar-left-junction ?╠
   cfw:fchar-right-junction ?╣
   cfw:fchar-top-junction ?╦
   cfw:fchar-top-left-corner ?╔
   cfw:fchar-top-right-corner ?╗)

  ;;; (Bindings) ;;;
  (define-key user/utilities-map (kbd "c") 'cfw:open-calendar-buffer))


(require-package '(:name calfw
                         :after (user/calfw-init)))
(require-package '(:name calfw-gcal
			 :type github
			 :pkgname "myuhe/calfw-gcal.el"
			 :depends (calfw)))


(provide 'apps/calfw)
;;; calfw.el ends here
