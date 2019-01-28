;;; packaging.el --- initialize package management -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'lib/net)


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


(provide 'lib/packaging)
;;; packaging.el ends here
