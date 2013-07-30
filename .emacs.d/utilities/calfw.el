;;; calfw --- calendar for Emacs
;;; Commentary:
;;; Code:

(defun dholm/calfw-init ()
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
         '(cfw:face-day-title ((t (:background ,base02))))
         '(cfw:face-annotation ((t (:inherit cfw:face-day-title :foreground ,yellow))))
         '(cfw:face-default-content ((t (:foreground ,green))))
         '(cfw:face-default-day ((t (:inherit cfw:face-day-title :weight bold))))
         '(cfw:face-disable ((t (:inherit cfw:face-day-title :foreground ,base01))))
         '(cfw:face-grid ((t (:foreground ,base01))))
         '(cfw:face-header ((t (:foreground ,base03 :background ,blue :weight bold))))
         '(cfw:face-holiday ((t (:background nil :foreground ,red :weight bold))))
         '(cfw:face-periods ((t (:foreground ,magenta))))
         '(cfw:face-select ((t (:background ,base03 :foreground ,magenta))))
         '(cfw:face-saturday ((t (:foreground ,base03 :background ,cyan))))
         '(cfw:face-sunday ((t (:foreground ,base03 :background ,red :weight bold))))
         '(cfw:face-title ((t (:inherit variable-pitch :foreground ,yellow :weight bold :height 2.0))))
         '(cfw:face-today ((t (:weight bold :background ,base02 :foreground nil))))
         '(cfw:face-today-title ((t (:foreground ,base03 :background ,yellow :weight bold))))
         '(cfw:face-toolbar ((t (:foreground ,base0 :background ,base02))))
         '(cfw:face-toolbar-button-off ((t (:foreground ,base03 :background ,yellow :weight bold))))
         '(cfw:face-toolbar-button-on ((t (:foreground ,base03 :background ,yellow :weight bold))))))))

  ;;; (Bindings) ;;;
  (define-key dholm/utilities-map (kbd "c") 'cfw:open-calendar-buffer))


(require-package '(:name calfw
                         :after (dholm/calfw-init)))
(require-package '(:name calfw-gcal
			 :type github
			 :pkgname "myuhe/calfw-gcal.el"
			 :depends (calfw)
			 :features calfw-gcal))


(provide 'utilities/calfw)
;;; calfw.el ends here
