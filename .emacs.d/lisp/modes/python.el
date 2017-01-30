;;; python.el --- initializes Python modes
;;; Commentary:
;;; Code:

(defun user--python-mode-hook ()
  "Python mode hook."
  ;; Load CEDET
  (user--python-mode-cedet-hook)

  (user/gnu-global-enable)

  ;; Enable virtualenv support.
  (when (feature-p 'pyvenv)
    (pyvenv-mode t))

  (when (feature-p 'pymacs)
    (pymacs-load "ropemacs" "rope-"))

  (when (feature-p 'anaconda-mode)
    (anaconda-mode t))

  ;; Enable Jedi
  (when (feature-p 'jedi)
    (jedi:setup))

  ;; Separate camel-case into separate words
  (subword-mode t)

  ;; ElDoc shows function documentation as you type
  (eldoc-mode t)

  ;; Enable YouCompleteMe.
  (user/ycmd-enable)

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
    (unless (or (user/auto-complete-p) (user/company-mode-p))
      (user/bind-key-local :code :auto-complete 'jedi:complete))))


(with-executable 'bpython
  (defun user/bpython-term ()
    "Launch or switch to a `bpython' buffer."
    (interactive)
    (if (not (get-buffer "*bpython*"))
        (progn
          (ansi-term "bpython" "bpython"))
      (switch-to-buffer "*bpython*"))))


(defun user--python-mode-cedet-hook ()
  "CEDET hook for Python mode."
  (with-feature 'semantic/wisent/python
    (user--cedet-hook)))


(defun user--jedi-config ()
  "Initialize jedi."
  (setq-default
   ;; Don't install Jedi's bindings.
   jedi:setup-keys nil
   ;; Automatically launch completion on dot.
   jedi:complete-on-dot t
   ;; Use popup package.
   jedi:tooltip-method '(popup)))


(defun user--python-config ()
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


(defun user--pymacs-config ()
  "Initialize PyMacs."
  (setq-default
   ropemacs-guess-project t
   ropemacs-enable-autoimport t
   ;; Don't generate an error on syntax errors.
   ropemacs-codeassist-maxfixes 3))


(defun user--python-environment-config ()
  "Initialize Python environment package."
  (setq-default
   ;; Locate of Python environment store.
   python-environment-directory (path-join *user-cache-directory*
                                           "python-environment")))


(defun user--python-mode-config ()
  "Initialize Python mode."
  ;;; (Packages) ;;;
  (use-package python
    :defer t
    :config (user--python-config))
  (use-package anaconda-mode
    :defer t)
  (use-package py-autopep8
    :defer t)
  (require-package '(:name ropemacs))
  (use-package pymacs
    :defer t)
  (require-package '(:name pylookup))
  (use-package nose
    :defer t)
  (use-package python-environment
    :defer t
    :config (user--python-environment-config))
  (use-package pyvenv
    :defer t)
  (use-package jedi
    :defer t
    :config (user--jedi-config))
  (when (feature-p 'helm)
    (use-package helm-pydoc
      :defer t))

  (add-interpreter-mode 'python-mode "python[0-9.]*")
  (add-hook 'python-mode-hook 'user--python-mode-hook))

(with-executable 'python
  (user--python-mode-config))


(provide 'modes/python)
;;; python.el ends here
