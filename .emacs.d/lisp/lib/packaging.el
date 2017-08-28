;;; packaging.el --- initialize package management -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'lib/net)


(defvar el-get-sources nil
  "List of package definitions for el-get.")
(defvar el-get-safe-mode nil
  "Start el-get in safe mode.")
(defcustom user--after-init-hook nil
  "Hook that is run after both Emacs and package manager have completed init."
  :group 'init
  :type 'hook)


(with-feature 'package
  (setq
   ;; Configure GNU/Emacs package repositories.
   package-archives
   '(("GNU ELPA"     . "http://elpa.gnu.org/packages/")
     ("MELPA Stable" . "http://stable.melpa.org/packages/")
     ("MELPA"        . "http://melpa.org/packages/")
     ("org"          . "http://orgmode.org/elpa/")
     ("marmalade"    . "http://marmalade-repo.org/packages/"))
   ;; Prefer MELPA Stable over GNU over MELPA.
   package-archive-priorities
   '(("MELPA Stable" . 20)
     ("GNU ELPA"     . 15)
     ("MELPA"        . 10)
     ("org"          . 5)
     ("marmalade"    . 0))))


;; Bootstrap `use-package'.
(package-initialize)
(unless (and (package-installed-p 'quelpa-use-package) (package-installed-p 'validate))
  (package-refresh-contents)
  (package-install 'quelpa-use-package)
  (package-install 'validate))

(eval-when-compile
  ;; Load use-package.
  (require 'quelpa-use-package)
  (require 'validate))

(use-package use-package
  :pin "MELPA"
  :config
  (use-package quelpa-use-package
    :config
    (validate-setq
     ;; Always ensure packages have been installed.
     use-package-always-ensure t
     ;; Only use quelpa for custom packages.
     quelpa-checkout-melpa-p nil
     ;; Only load quelpa on demand.
     quelpa-use-package-inhibit-loading-quelpa t)

    ;; Protect quelpa recipes when forcing ensure.
    (quelpa-use-package-activate-advice)))


(defun user--el-get-init ()
  "Initialize el-get."
  (validate-setq
   el-get-safe-mode t)

  (add-to-list 'load-path (path-join *user-el-get-directory* "el-get")))

(defun user--el-get-config ()
  "Configure el-get."
  (add-to-list 'el-get-recipe-path
               (path-join *user-el-get-directory* "el-get" "recipes")))

;; Configure and load el-get
(user--el-get-init)
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))
(user--el-get-config)


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


(defun user--el-get-config ()
  "Initialize el-get as package manager."
  (validate-setq
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
    (run-hooks 'user--after-init-hook))

  ;; Make sure el-get is registered so that el-get-cleanup doesn't remove it
  (require-package '(:name el-get)))


(defun user--nil-package-config ()
  "Initialize nil as package manager."
  (defun require-package (package)
    "Add the specified PACKAGE to nil.")

  (defun user/package-list ()
    "Get the list of registered packages from nil.")

  (defun user/sync-packages ()
    "Sync all required packages."
    (run-hooks 'user--after-init-hook)))


(cond
 ((featurep 'el-get) (user--el-get-config))
 (t (user--nil-package-config)))


(provide 'lib/packaging)
;;; packaging.el ends here
