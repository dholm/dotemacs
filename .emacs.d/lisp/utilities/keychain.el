;;; keychain.el --- Support for ssh keys through keychain -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package keychain-environment
  :if (executable-find "keychain"))


(provide 'utilities/keychain)
;;; keychain.el ends here
