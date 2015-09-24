;;; packaging.el --- initialize package management
;;; Commentary:
;;; Code:

(defvar el-get-sources nil
  "List of package definitions for el-get.")
(defvar el-get-safe-mode nil
  "Start el-get in safe mode.")
(defcustom user/after-init-hook nil
  "Hook that is run after both Emacs and package manager have completed init."
  :group 'init
  :type 'hook)


(with-feature 'package
  ;; Configure ELPA repositories
  (add-many-to-list
   'package-archives
   '("gnu" . "http://elpa.gnu.org/packages/")
   '("marmalade" . "http://marmalade-repo.org/packages/")
   '("melpa" . "http://melpa.milkbox.net/packages/")))


;; Configure and load el-get
(add-to-list 'load-path (path-join *user-el-get-directory* "el-get"))
(unless (require 'el-get nil 'noerror)
  (setq el-get-safe-mode t)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))
    (el-get-elpa-build-local-recipes)))


(defun user/package-as-el-get (package)
  "Convert PACKAGE into el-get format."
  (append
   `(:name ,(plist-get package :name))
   (when (plist-member package :before)
     `(:before ,(plist-get package :before)))
   (when (plist-member package :after)
     `(:after ,(plist-get package :after)))
   (when (plist-member package :prepare)
     `(:prepare ,(plist-get package :prepare)))
   (when (plist-member package :type)
     (append
      `(:type ,(plist-get package :type))
      (cond
       ((plist-member package :url) `(:url ,(plist-get package :url)))
       ((plist-member package :pkgname) `(:pkgname ,(plist-get package :pkgname))))))))


(defun user/el-get-init ()
  "Initialize el-get as package manager."
  (setq-default
   el-get-user-package-directory (path-join user-emacs-directory "init")
   el-get-verbose el-get-safe-mode
   ;; Don't produce system notifications.
   el-get-notify-type 'message)

  (defun require-package (package)
    "Add the specified PACKAGE to el-get-sources."
    (add-to-list 'el-get-sources (user/package-as-el-get package)))

  (defun user/package-list ()
    "Get the list of registered packages from el-get."
    (mapcar 'el-get-as-symbol (mapcar 'el-get-source-name el-get-sources)))

  (defun user/sync-packages ()
    "Sync all required packages."
    (let ((package-list (user/package-list)))
      (if el-get-safe-mode
          (el-get 'sync package-list)
        (el-get nil package-list)))
    (when (featurep 'package)
      (package-initialize))
    (run-hooks 'user/after-init-hook))

  ;; Make sure el-get is registered so that el-get-cleanup doesn't remove it
  (require-package '(:name el-get)))


(defun user/nil-package-init ()
  "Initialize nil as package manager."
  (defun require-package (package)
    "Add the specified PACKAGE to nil.")

  (defun user/package-list ()
    "Get the list of registered packages from nil.")

  (defun user/sync-packages ()
    "Sync all required packages."
    (run-hooks 'user/after-init-hook)))


(cond
 ((featurep 'el-get) (user/el-get-init))
 (t (user/nil-package-init)))


(provide 'lib/packaging)
;;; packaging.el ends here
