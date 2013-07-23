;;; xml --- initializes XML modes
;;; Commentary:
;;; Code:

;; Enable magic-mode to detect XML
(setq magic-mode-alist (cons '("<\\?xml " . nxml-mode) magic-mode-alist))


(defun dholm/xml-mode-hook ()
  ;; Use nxml-mode for XML
  (nxml-mode)
  ;; Configure nxml auto completion
  (setq nxml-slash-auto-complete-flag t))

(add-hook 'xml-mode-hook 'dholm/xml-mode-hook)


(provide 'modes/xml)
;;; xml.el ends here
