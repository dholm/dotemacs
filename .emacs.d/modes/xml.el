;;; xml --- initializes XML modes
;;; Commentary:
;;; Code:

(defun dholm/xml-mode-hook ()
  "XML mode hook."
  ;; Use nxml-mode for XML
  (nxml-mode t)
  ;; Configure nxml auto completion
  (setq nxml-slash-auto-complete-flag t))


(defun dholm/xml-mode-init ()
  "Initialize xml mode."
  (add-hook 'xml-mode-hook 'dholm/xml-mode-hook)
  (setq
   ;; Enable magic-mode to detect XML
   magic-mode-alist (cons '("<\\?xml " . nxml-mode) magic-mode-alist)))

(dholm/xml-mode-init)


(provide 'modes/xml)
;;; xml.el ends here
