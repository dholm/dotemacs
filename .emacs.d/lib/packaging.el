;; Configure ELPA repositories
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)


;; Configure and load el-get
(add-to-list 'load-path (path-join *user-el-get-directory* "el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))


(defvar el-get-packages nil "List of el-get packages to sync")

(setq
 el-get-user-package-directory (path-join user-emacs-directory "init")
 el-get-verbose t)


(defmacro require-package (package)
  "Adds the specified package to el-get-sources"
  `(setq el-get-sources (append el-get-sources '(,package))))


(defun dholm/package-list ()
  "Returns the list of registered packages"
  (append el-get-packages
          (mapcar 'el-get-as-symbol (mapcar 'el-get-source-name el-get-sources))))


(defun dholm/sync-packages ()
  "Sync all required packages"
  (el-get 'sync (dholm/package-list)))


;; Make sure el-get is registered so that el-get-cleanup doesn't remove it
(require-package (:name el-get
			:pkgname "dimitri/el-get"
			:type github
			:branch "master"))


(provide 'lib/packaging)
