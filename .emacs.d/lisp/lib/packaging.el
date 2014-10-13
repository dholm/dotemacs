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


;; Configure ELPA repositories
(with-feature 'package
  (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                           ("marmalade" . "http://marmalade-repo.org/packages/")
                           ("melpa" . "http://melpa.milkbox.net/packages/")))
  (package-initialize))


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


(when (featurep 'el-get)
  (setq-default
   el-get-user-package-directory (path-join user-emacs-directory "init")
   el-get-verbose el-get-safe-mode))


(defun require-package (package)
  "Add the specified PACKAGE to el-get-sources."
  (setq el-get-sources (append el-get-sources `(,package))))


(defun user/package-list ()
  "Get the list of registered packages."
  (mapcar 'el-get-as-symbol (mapcar 'el-get-source-name el-get-sources)))


(defun user/sync-packages ()
  "Sync all required packages."
  (let ((package-list (user/package-list)))
    (if el-get-safe-mode
        (el-get 'sync package-list)
      (el-get nil package-list))
    (run-hooks 'user/after-init-hook)))


(defun user/load-from-package (package &rest path)
  "Load file from PACKAGE at PATH."
  (when (el-get-package-is-installed package)
    (let ((package-path (el-get-load-path package)))
      (load (path-join package-path path) :noerror))))


;; Make sure el-get is registered so that el-get-cleanup doesn't remove it
(require-package '(:name el-get))


(provide 'lib/packaging)
;;; packaging.el ends here
