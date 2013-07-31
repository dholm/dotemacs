;;; xml.el --- initializes XML modes
;;; Commentary:
;;; Code:

(defun dholm/nxml-mode-hook ()
  "XML mode hook."
  ;; Configure nxml auto completion
  (require 'auto-complete-nxml)
  (setq nxml-slash-auto-complete-flag t))


(defun dholm/xml-mode-init ()
  "Initialize xml mode."
  ;; Use nxml-mode for XML
  (fset 'xml-mode 'nxml-mode)
  (add-hook 'nxml-mode-hook 'dholm/nxml-mode-hook)
  (setq
   ;; Enable magic-mode to detect XML
   magic-mode-alist (cons '("<\\?xml " . nxml-mode) magic-mode-alist)))

(dholm/xml-mode-init)


(defun dholm/auto-complete-nxml-init ()
  "Initialize auto completion for nxml mode.")

(require-package '(:name auto-complete-nxml :after (dholm/auto-complete-nxml-init)))


(provide 'modes/xml)
;;; xml.el ends here
