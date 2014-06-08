;;; python.el --- initializes Python modes
;;; Commentary:
;;; Code:

(defun user/python-mode-hook ()
  "Python mode hook."
  ;; Load CEDET
  (user/python-mode-cedet-hook)

  ;; Enable virtualenv support.
  (when (feature-p 'pyvenv)
    (pyvenv-mode t))

  ;; Load ropemacs
  (when (feature-p 'pymacs)
    (pymacs-load "ropemacs" "rope-")
    ;; Auto-completion sources
    (add-ac-sources 'ac-source-ropemacs))

  ;; Enable Jedi
  (when (el-get-package-is-installed 'jedi)
    (jedi:setup))

  ;; Separate camel-case into separate words
  (subword-mode t)

  ;; ElDoc shows function documentation as you type
  (eldoc-mode t)

  ;;; (Bindings) ;;;
  (when (feature-p 'nose)
    (user/bind-key-local :code :test 'nosetests-all))
  (when (feature-p 'pyvenv)
    (user/bind-key-local :code :virtual 'pyvenv-workon))
  (when (feature-p 'jedi)
    (user/bind-key-local :nav :follow-symbol 'jedi:goto-definition)
    (user/bind-key-local :nav :go-back 'jedi:goto-definition-pop-marker)
    (when (feature-p 'helm)
      (user/bind-key-local :nav :references 'helm-jedi-related-names))
    (user/bind-key-local :doc :describe 'jedi:show-doc)
    (user/bind-key-local :code :auto-complete 'jedi:complete)))


(defun user/python-mode-cedet-hook ()
  "CEDET hook for Python mode."
  (with-feature 'semantic/wisent/python
    (user/cedet-hook)))


(defun user/jedi-init ()
  "Initialize jedi."
  (setq-default
   ;; Don't install Jedi's bindings.
   jedi:setup-keys nil
   ;; Automatically launch completion on dot.
   jedi:complete-on-dot t
   ;; Use popup package.
   jedi:tooltip-method '(popup)))


(defun user/python-init ()
  "Initialize python."
  (setq-default
   ;; Don't try to guess the indentation.
   python-indent-guess-indent-offset nil)

  (with-executable 'ipython
    (setq-default
     ;; Set IPython as default interpreter.
     python-shell-interpreter "ipython"
     python-shell-interpreter-args ""
     python-shell-prompt-regexp "In \\[[0-9]+\\]: "
     python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
     python-shell-completion-setup-code "from IPython.core.completerlib import module_completion"
     python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n"
     python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"))

  (add-auto-mode 'python-mode "SConstruct" "SConscript"))


(defun user/pymacs-init ()
  "Initialize PyMacs."
  (setq-default
   ropemacs-guess-project t
   ropemacs-enable-autoimport t
   ;; Don't generate an error on syntax errors.
   ropemacs-codeassist-maxfixes 3))


(defun user/python-environment-init ()
  "Initialize Python environment package."
  (setq-default
   ;; Locate of Python environment store.
   python-environment-directory (path-join *user-cache-directory*
                                           "python-environment")))


(defun user/python-mode-init ()
  "Initialize Python mode."
  ;;; (Packages) ;;;
  (require-package '(:name python :after (user/python-init)))
  (require-package '(:name pymacs))
  (require-package '(:name rope))
  (require-package '(:name pylookup))
  (require-package '(:name nose))
  (require-package '(:name python-environment :after (user/python-environment-init)))
  (require-package '(:name pyvenv))
  (require-package '(:name jedi :after (user/jedi-init)))

  (add-interpreter-mode 'python-mode "python")
  (add-hook 'python-mode-hook 'user/python-mode-hook))

(with-executable 'python
  (user/python-mode-init))


(provide 'modes/python)
;;; python.el ends here
