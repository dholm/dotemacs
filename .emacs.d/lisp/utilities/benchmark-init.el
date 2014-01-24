;;; benchmark-init.el --- Benchmark of Emacs initialization
;;; Commentary:
;;; Code:

(require-package '(:name benchmark-init
                         :type github
                         :depends (ctable)
                         :pkgname "dholm/benchmark-init-el"))


(provide 'utilities/benchmark-init)
;;; benchmark-init.el ends here
