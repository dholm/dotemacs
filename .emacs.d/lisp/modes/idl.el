;;; idl.el --- Initializes IDL mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package cc-mode
  :ensure nil
  :defer
  ;; Flatbuffers schema file.
  :mode (("fbs\\'" . idl-mode)))


(provide 'modes/idl)
;;; idl.el ends here
