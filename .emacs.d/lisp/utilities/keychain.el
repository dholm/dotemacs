;;; keychain.el --- Support for ssh keys through keychain
;;; Commentary:
;;; Code:

(with-executable 'keychain
  (use-package keychain-environment
    :ensure t))


(provide 'utilities/keychain)
;;; keychain.el ends here
