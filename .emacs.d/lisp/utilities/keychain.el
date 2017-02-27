;;; keychain.el --- Support for ssh keys through keychain -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(with-executable 'keychain
  (use-package keychain-environment))


(provide 'utilities/keychain)
;;; keychain.el ends here
