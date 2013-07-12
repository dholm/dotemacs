;;; (Initialization) ;;;

(fset 'xml-mode 'nxml-mode)

;; Enable magic-mode to detect XML
(setq magic-mode-alist (cons '("<\\?xml " . nxml-mode) magic-mode-alist))


(defun dholm/nxml-mode-hook ()
  ;; Configure nxml auto completion
  (setq nxml-slash-auto-complete-flag t))

(add-hook 'nxml-mode-hook 'dholm/nxml-mode-hook)


(provide 'modes/xml)
