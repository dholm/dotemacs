;;; xml.el --- initializes XML modes
;;; Commentary:
;;; Code:

(defun user/nxml-mode-hook ()
  "XML mode hook."
  (with-feature 'auto-complete-nxml
    ;; Configure nxml auto completion
    (setq nxml-slash-auto-complete-flag t))

  ;; Register file types with find-file-in-project
  (after-load 'find-file-in-project
    (user/ffip-local-patterns "*.xml")))


(defun user/auto-complete-nxml-init ()
  "Initialize auto completion for nxml mode.")


(defun user/xml-mode-init ()
  "Initialize xml mode."
  ;; Use nxml-mode for XML
  (fset 'xml-mode 'nxml-mode)
  (add-hook 'nxml-mode-hook 'user/nxml-mode-hook)
  (add-magic-mode 'nxml-mode "<\\?xml ")

  (require-package '(:name auto-complete-nxml :after (user/auto-complete-nxml-init))))

(user/xml-mode-init)


(provide 'modes/xml)
;;; xml.el ends here
