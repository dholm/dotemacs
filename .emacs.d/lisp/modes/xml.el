;;; xml.el --- initializes XML modes
;;; Commentary:
;;; Code:

(defun user--xml-mode-common-hook ()
  "XML common mode hook."
  ;; Outline XML support.
  (setq
   outline-regexp "^[ \t]*\<[a-zA-Z]+")
  (outline-minor-mode t)

  ;;; (Bindings) ;;;
  (user/bind-key-local :code :tidy 'user/xml-tidy-buffer-or-region))


(defun user--nxml-mode-hook ()
  "XML mode hook."
  (user--xml-mode-common-hook)

  (when (user/auto-complete-p)
    (with-feature 'auto-complete-nxml
      ;; Configure nxml auto completion
      (setq nxml-slash-auto-complete-flag t)))

  (with-feature 'sgml-mode
    (hs-minor-mode t))

  (with-feature 'tidy
    (tidy-build-menu nxml-mode-map))

  ;;; (Bindings) ;;;
  (local-set-key (kbd "<return>") 'newline-and-indent))


(defun user--dtd-mode-hook ()
  "DTD mode hook."
  (user--xml-mode-common-hook)

  (with-feature 'tidy
    (tidy-build-menu dtd-mode-map)))


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


(defun user--tdtd-config ()
  "Initialize tdtd."
  ;; DTD modes.
  (add-auto-mode 'dtd-mode "\\.dtd$")

  ;;; (Hooks) ;;;
  (add-hook 'dtd-mode-hook 'user--dtd-mode-hook))


(defun user--xml-mode-config ()
  "Initialize xml mode."
  ;; Use nxml-mode for XML
  (fset 'xml-mode 'nxml-mode)

  ;; XML modes.
  (add-magic-mode 'nxml-mode "<\\?xml")
  (add-auto-mode 'nxml-mode
                 "\\.xml$" "\\.xsd$" "\\.xslt$" "\\.rss$" "\\.svg$" "\\.plist$"
                 "\\.rng$" "\\.rnc$")

  ;; Enable hide/show support for XML.
  (add-to-list
   'hs-special-modes-alist
   '(nxml-mode
     "<!--\\|<[^/>]>\\|<[^/][^>]*[^/]>"
     ""
     ;; Comment start.
     "<!--"
     sgml-skip-tag-forward nil))

  ;;; (Hooks) ;;;
  (add-hook 'nxml-mode-hook 'user--nxml-mode-hook)

  ;;; (Packages) ;;;
  (require-package '(:name tdtd :after (user--tdtd-config)))
  (use-package auto-complete-nxml
    :defer t
    :requires auto-complete
    :config
    (add-ac-modes 'nxml-mode)))

(user--xml-mode-config)


(provide 'modes/xml)
;;; xml.el ends here
