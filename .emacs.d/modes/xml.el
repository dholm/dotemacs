;;; xml.el --- initializes XML modes
;;; Commentary:
;;; Code:

(defun user/nxml-mode-hook ()
  "XML mode hook."
  ;; Configure nxml auto completion
  (require 'auto-complete-nxml)
  (setq nxml-slash-auto-complete-flag t))


(defun user/xml-mode-init ()
  "Initialize xml mode."
  ;; Use nxml-mode for XML
  (fset 'xml-mode 'nxml-mode)
  (add-hook 'nxml-mode-hook 'user/nxml-mode-hook)
  (setq
   ;; Enable magic-mode to detect XML
   magic-mode-alist (cons '("<\\?xml " . nxml-mode) magic-mode-alist)))

(user/xml-mode-init)


(defun user/auto-complete-nxml-init ()
  "Initialize auto completion for nxml mode.")

(require-package '(:name auto-complete-nxml :after (user/auto-complete-nxml-init)))


(provide 'modes/xml)
;;; xml.el ends here
