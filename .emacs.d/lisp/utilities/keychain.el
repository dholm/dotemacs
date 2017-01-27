;;; keychain.el --- Support for ssh keys through keychain
;;; Commentary:
;;; Code:

(with-executable 'keychain
  (req-package keychain-environment))


(provide 'utilities/keychain)
;;; keychain.el ends here
