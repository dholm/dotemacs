;;; scrolling.el --- Configure scrolling in Emacs buffers -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user--scrolling-config ()
  "Configure Emacs buffer scrolling."
  (validate-setq
   ;; Set distance in line from margin before scrolling commences.
   scroll-margin 5
   ;; Prevent cursor from jumping to center of window on scroll.
   scroll-conservatively 100000
   ;; Try to maintain screen position when scrolling entire pages.
   scroll-preserve-screen-position t)

  ;;; (Bindings) ;;;
  (user/bind-key-global :emacs :recenter 'recenter))

(use-package mwheel
  :ensure nil
  :if window-system
  :config
  (validate-setq
   ;; Scroll five lines when using mouse wheel.
   mouse-wheel-scroll-amount '(5 ((shift) . 5))
   ;; Use constant speed when scrolling with mouse wheel.
   mouse-wheel-progressive-speed nil
   ;; Scroll the window that the cursor is over.
   mouse-wheel-follow-mouse t))

(user--scrolling-config)


(provide 'ux/scrolling)
;;; scrolling.el ends here
