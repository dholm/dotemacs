;;; xml.el --- initializes XML modes
;;; Commentary:
;;; Code:

(defun user/nxml-mode-hook ()
  "XML mode hook."
  (with-feature 'auto-complete-nxml
    ;; Configure nxml auto completion
    (setq nxml-slash-auto-complete-flag t))

  (with-feature 'tidy
    (tidy-build-menu nxml-mode-map))

  ;;; (Bindings) ;;;
  (local-set-key (kbd "<return>") 'newline-and-indent)
  (user/bind-key-local :code :tidy 'user/xml-tidy-buffer-or-region))


(defun user/xml-tidy-buffer-or-region ()
  "Pretty print XML in current buffer or region."
  (interactive)
  (let ((begin (if (use-region-p) (region-beginning) (point-min)))
        (end (if (use-region-p) (region-end) (point-max))))
    (user/xml-tidy-region begin end)))

(defun user/xml-tidy-region (begin end)
  "Pretty print XML between BEGIN and END."
  (interactive "r")
  (save-excursion
    (nxml-mode)
    (goto-char begin)
    (while (search-forward-regexp "\>[ \\t]*\<" nil t)
      (backward-char)
      (insert "\n"))
    (indent-region begin end)))


(defun user/auto-complete-nxml-init ()
  "Initialize auto completion for nxml mode."
  (add-ac-modes 'nxml-mode))


(defun user/xml-mode-init ()
  "Initialize xml mode."
  ;; Use nxml-mode for XML
  (fset 'xml-mode 'nxml-mode)

  ;; XML modes.
  (add-magic-mode 'nxml-mode "<\\?xml")
  (add-auto-mode 'nxml-mode "\\.xsd$" "\\.xslt$" "\\.rss$")

  ;;; (Hooks) ;;;
  (add-hook 'nxml-mode-hook 'user/nxml-mode-hook)

  ;;; (Packages) ;;;
  (require-package '(:name auto-complete-nxml
                           :after (user/auto-complete-nxml-init))))

(user/xml-mode-init)


(provide 'modes/xml)
;;; xml.el ends here
