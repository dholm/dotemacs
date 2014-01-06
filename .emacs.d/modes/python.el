;;; python.el --- initializes Python modes
;;; Commentary:
;;; Code:

(defun user/python-mode-hook ()
  "Python mode hook."
  (unless (derived-mode-p 'prog-mode)
    (run-hooks 'prog-mode-hook))

  ;; Load CEDET
  (user/python-mode-cedet-hook)

  ;; Load ropemacs
  (when (el-get-package-is-installed 'pymacs)
    (pymacs-load "ropemacs" "rope-"))

  (when (el-get-package-is-installed 'elpy)
    (elpy-enable)
    (when *has-ipython*
      (elpy-use-ipython)))

  ;; Separate camel-case into separate words
  (subword-mode t)

  ;; ElDoc shows function documentation as you type
  (eldoc-mode t)

  ;; Bind electric backspace to del which translates to backspace in
  ;; terminals.
  (define-key python-mode-map (kbd "DEL") 'py-electric-backspace)

  ;; Auto-completion sources
  (add-ac-sources 'ac-source-ropemacs)

  ;; Register file types with find-file-in-project
  (after-load 'find-file-in-project
    (user/ffip-local-patterns "*.py")))


(defun user/python-mode-cedet-hook ()
  "CEDET hook for Python mode."
  (user/cedet-hook)
  (require 'semantic/wisent/python))


(defun user/jedi-init ()
  "Initialize jedi."
  (setq-default
   ;; Use default Jedi bindings.
   jedi:setup-keys t
   ;; Automatically launch completion on dot.
   jedi:complete-on-dot t
   ;; Use popup package.
   jedi:tooltip-method '(popup))

  ;;; (Faces) ;;;
  (after-load 'solarized-theme
    (solarized-with-values
      (eval
       `(custom-theme-set-faces
         'solarized
         '(jedi:highlight-function-argument ((t (:inherit bold)))))))))


(defun user/elpy-init ()
  "Initialize Elpy."
  (when (el-get-package-is-installed 'jedi)
    (user/jedi-init))

  (setq-default
   elpy-rpc-backend "jedi"))


(defun user/pymacs-init ()
  "Initialize PyMacs."
  (setq-default
   ropemacs-enable-autoimport t))


(defun user/python-mode-init ()
  "Initialize Python mode."
  (require-package '(:name python))
  (require-package '(:name pymacs))
  (require-package '(:name pylookup))
  (require-package '(:name elpy :after (user/elpy-init)))

  (add-interpreter-mode 'python-mode "python")
  (add-hook 'python-mode-hook 'user/python-mode-hook))


(when *has-python*
  (user/python-mode-init))


(provide 'modes/python)
;;; python.el ends here
