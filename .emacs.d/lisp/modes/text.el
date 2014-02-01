;;; text.el --- text mode support
;;; Commentary:
;;; Code:

(defun user/text-mode-hook ()
  "Text mode hook."
  (setq
   ;; Indent using spaces by default
   indent-tabs-mode nil
   ;; Colons are followed by two spaces
   colon-double-space t
   ;; Sentences end after two spaces
   sentence-end-double-space t
   ;; When using fill-paragraph or auto-fill-mode break lines at 79 characters
   ;; by default.
   fill-column 79)
  ;; Automatically break long lines
  (auto-fill-mode t)
  ;; Run spell-checker
  (flyspell-mode)
  ;; Delete trailing whitespace on save
  (add-hook 'write-contents-functions 'delete-trailing-whitespace nil t)
  ;; Enable dtrt-indent to attempt to identify the indentation rules used
  (after-load 'dtrt-indent
    (dtrt-indent-mode t))

  ;;; (Bindings) ;;;
  (when (el-get-package-is-installed 'synosaurus)
    (define-key user/code-map (kbd "t") 'synosaurus-choose-and-replace)))


(defun user/synosaurus-init ()
  "Initialize Synosaurus thesaurus."
  (setq-default
   ;; Use popup to display options.
   synosaurus-choose-method 'popup
   ;; Use WordNet for lookups.
   synosaurus-lookup-function 'wordnet-lookup))


(defun user/text-mode-init ()
  "Initialize generic text editing mode."
  (add-hook 'text-mode-hook 'user/text-mode-hook)

  ;;; (Packages) ;;;
  (require-package '(:name synosaurus
                           :after (user/synosaurus-init)
                           :type github
                           :pkgname "rootzlevel/synosaurus"
                           :prepare (progn
                                      (autoload 'synosaurus-lookup "synosaurus")
                                      (autoload 'synosaurus-choose-and-replace "synosaurus")
                                      (autoload 'openthesaurus-lookup "synosaurus-openthesaurus")
                                      (autoload 'wordnet-lookup "synosaurus-wordnet")))))


(user/text-mode-init)


(provide 'modes/text)
;;; text.el ends here
