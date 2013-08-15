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

  ;;; (Faces) ;;;
  (after-load 'solarized-theme
    (solarized-with-values
      (eval
       `(custom-theme-set-faces
         'solarized
         '(cfw:face-day-title ((t (:background ,solarized-hl))))
         '(cfw:face-annotation ((t (:inherit cfw:face-day-title :foreground ,yellow))))
         '(cfw:face-default-content ((t (:foreground ,green))))
         '(cfw:face-default-day ((t (:inherit cfw:face-day-title :weight bold))))
         '(cfw:face-disable ((t (:inherit cfw:face-day-title :foreground ,solarized-comment))))
         '(cfw:face-grid ((t (:foreground ,solarized-comment))))
         '(cfw:face-header ((t (:foreground ,blue-hc :background ,blue-lc :weight bold))))
         '(cfw:face-holiday ((t (:background nil :foreground ,red :weight bold))))
         '(cfw:face-periods ((t (:foreground ,magenta))))
         '(cfw:face-select ((t (:background ,magenta-hc :foreground ,magenta-lc))))
         '(cfw:face-saturday ((t (:foreground ,cyan-hc :background ,cyan-lc))))
         '(cfw:face-sunday ((t (:foreground ,red-hc :background ,red-lc :weight bold))))
         '(cfw:face-title ((t (:inherit variable-pitch :foreground ,yellow
                                        :weight bold :height 2.0))))
         '(cfw:face-today ((t (:weight bold :background ,solarized-hl :foreground nil))))
         '(cfw:face-today-title ((t (:foreground ,yellow-hc :background ,yellow-lc
                                                 :weight bold))))
         '(cfw:face-toolbar ((t (:foreground ,solarized-fg :background ,solarized-hl))))
         '(cfw:face-toolbar-button-off ((t (:foreground ,yellow-hc :background ,yellow-lc
                                                        :weight bold))))
         '(cfw:face-toolbar-button-on ((t (:foreground ,yellow-lc :background ,yellow-hc
                                                       :weight bold))))))))

  ;;; (Bindings) ;;;
  (define-key user/utilities-map (kbd "c") 'cfw:open-calendar-buffer))


(require-package '(:name calfw
                         :after (user/calfw-init)))
(require-package '(:name calfw-gcal
			 :type github
			 :pkgname "myuhe/calfw-gcal.el"
			 :depends (calfw)
			 :features calfw-gcal))


(provide 'apps/calfw)
;;; calfw.el ends here
