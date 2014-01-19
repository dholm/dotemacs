;;; python.el --- initializes Python modes
;;; Commentary:
;;; Code:

(defun user/python-mode-hook ()
  "Python mode hook."
  ;; Load CEDET
  (user/python-mode-cedet-hook)

  ;; Load ropemacs
  (when (el-get-package-is-installed 'pymacs)
    (pymacs-load "ropemacs" "rope-")
    ;; Auto-completion sources
    (add-ac-sources 'ac-source-ropemacs))

  ;; Enable Jedi
  (when (el-get-package-is-instsalled 'jedi)
    (jedi:setup))

  ;; Separate camel-case into separate words
  (subword-mode t)

  ;; ElDoc shows function documentation as you type
  (eldoc-mode t)

  ;; Register file types with find-file-in-project
  (after-load 'find-file-in-project
    (user/ffip-local-patterns "*.py"))

  ;;; (Bindings) ;;;
  ;; Bind electric backspace to del which translates to backspace in
  ;; terminals.
  (define-key python-mode-map (kbd "DEL") 'py-electric-backspace)
  (when (el-get-package-is-installed 'nose)
    (define-key user/code-map (kbd "t") 'nosetests-all))
  (when (el-get-package-is-installed 'virtualenv)
    (define-key user/code-map (kbd "v") 'virtualenv-workon)))


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


(defun user/python-init ()
  "Initialize python."
  (when *has-ipython*
    (setq-default
     python-shell-interpreter "ipython"
     python-shell-interpreter-args ""
     python-shell-prompt-regexp "In \\[[0-9]+\\]: "
     python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
     python-shell-completion-setup-code "from IPython.core.completerlib import module_completion"
     python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n"
     python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")))


(defun user/pymacs-init ()
  "Initialize PyMacs."
  (setq-default
   ropemacs-guess-project t
   ropemacs-enable-autoimport t
   ;; Don't generate an error on syntax errors.
   ropemacs-codeassist-maxfixes 3))


(defun user/python-mode-init ()
  "Initialize Python mode."
  ;;; (Packages) ;;;
  (require-package '(:name python :after (user/python-init)))
  (require-package '(:name pymacs))
  (require-package '(:name rope))
  (require-package '(:name pylookup))
  (require-package '(:name nose))
  (require-package '(:name virtualenv))
  (require-package '(:name jedi :after (user/jedi-init)))

  (add-interpreter-mode 'python-mode "python")
  (add-hook 'python-mode-hook 'user/python-mode-hook))

(when *has-python*
  (user/python-mode-init))


(provide 'modes/python)
;;; python.el ends here
