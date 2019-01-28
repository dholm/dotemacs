;;; packaging.el --- initialize package management -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'lib/net)


(defvar el-get-safe-mode nil
  "Start el-get in safe mode.")

(with-feature 'package
  (setq
   ;; Configure GNU/Emacs package repositories.
   package-archives
   '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
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
  (validate-setq
   ;; Always ensure packages have been installed.
   use-package-always-ensure t
   ;; Hooks are verbatim.
   use-package-hook-name-suffix nil)

  (use-package quelpa-use-package
    :config
    (validate-setq
     ;; Only use quelpa for custom packages.
     quelpa-checkout-melpa-p nil
     ;; Only load quelpa on demand.
     quelpa-use-package-inhibit-loading-quelpa t)

    ;; Protect quelpa recipes when forcing ensure.
    (quelpa-use-package-activate-advice))

  ;; Support using keys from init-bindings by using (:key <group> <function>).
  (push :bind-wrap (cdr (member :bind use-package-keywords)))
  (push :bind*-wrap (cdr (member :bind* use-package-keywords)))
  (defun use-package-normalize-bind-wrap (name keyword args)
    (let ((arg args)
          args*)
      (while arg
        (let ((x (car arg)))
          (cond
           ;; ((:key :category :function) . COMMAND)
           ((and (consp x)
                 (consp (car x))
                 (equal (caar x) :key))
            (setq args* (nconc args*
                               (list (cons (apply 'user/get-key (cdar x))
                                     (cdar arg)))))
            (setq arg (cdr arg)))
           ;; (KEY . COMMAND)
           ((and (consp x)
                 (or (stringp (car x))
                     (vectorp (car x)))
                 (or (use-package-recognize-function (cdr x) t #'stringp)))
            (setq args* (nconc args* (list x)))
            (setq arg (cdr arg)))
           ;; Nested list.
           ((listp x)
            (setq args*
                  (nconc args* (use-package-normalize/:bind-wrap name keyword x)))
            (setq arg (cdr arg)))
           (t
            (setq args* (nconc args* (list x)))
            (setq arg (cdr arg))))))
      (use-package-normalize/:bind name keyword args*)))
  (defalias 'use-package-normalize/:bind-wrap 'use-package-normalize-bind-wrap)
  (defalias 'use-package-normalize/:bind*-wrap 'use-package-normalize-bind-wrap)
  (defun use-package-handler/:bind-wrap (name keyword arg rest state)
    (use-package-handler/:bind name keyword arg rest state))
  (defun use-package-handler/:bind*-wrap (name keyword arg rest state)
    (use-package-handler/:bind name keyword arg rest state 'bind-keys*)))


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


(use-package el-get
  :ensure
  :pin "MELPA"
  :init
  (validate-setq
   el-get-safe-mode t)
  :config
  (validate-setq
   el-get-user-package-directory (path-join user-emacs-directory "init")
   el-get-verbose el-get-safe-mode
   ;; Don't produce system notifications.
   el-get-notify-type 'message)

  (use-package use-package-el-get
    :ensure
    :config
    (use-package-el-get-setup))

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
        (el-get nil package-list))))

  (require-package '(:name el-get))
  (add-to-list 'el-get-recipe-path
               (path-join *user-el-get-directory* "el-get" "recipes")))


(provide 'lib/packaging)
;;; packaging.el ends here
