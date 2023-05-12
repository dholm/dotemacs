;;; python.el --- initializes Python modes -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user--python-mode-hook ()
  "Python mode hook."
  ;; Load CEDET
  (user--python-mode-cedet-hook)

  (user/tags-try-enable)

  ;; Enable virtualenv support.
  (when (feature-p 'pyvenv)
    (pyvenv-mode t))

  (when (feature-p 'anaconda-mode)
    (anaconda-mode t))

  ;; Enable smart parenthesis handling.
  (user/smartparens-enable)

  ;; Separate camel-case into separate words
  (subword-mode t)

  ;; ElDoc shows function documentation as you type
  (eldoc-mode t)

  ;;; (Bindings) ;;;
  (when (feature-p 'pyvenv)
    (user/bind-key-local :code :virtual 'pyvenv-workon))
  (when (feature-p 'lsp-pyright)
    (require 'lsp-pyright)))


(defun user--python-mode-cedet-hook ()
  "CEDET hook for Python mode."
  (with-feature 'semantic/wisent/python
    (user--cedet-hook)))


(use-package python
  :if (executable-find "python")
  :defer
  :mode ("SCon\(struct\|script\)$" . python-mode)
  :interpreter ("python[0-9.]*" . python-mode)
  :hook
  (python-mode-hook . user--python-mode-hook)
  :config
  (validate-setq
   ;; Don't try to guess the indentation.
   python-indent-guess-indent-offset nil)

  (with-executable 'ipython3
    (validate-setq
     ;; Set IPython as default interpreter.
     python-shell-interpreter "ipython3"
     python-shell-interpreter-args ""
     python-shell-prompt-regexp "In \\[[0-9]+\\]: "
     python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
     python-shell-completion-setup-code "from IPython.core.completerlib import module_completion"
     python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n"
     python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"))

  (with-executable 'bpython
    (defun user/bpython-term ()
      "Launch or switch to a `bpython' buffer."
      (interactive)
      (if (not (get-buffer "*bpython*"))
          (progn
            (ansi-term "bpython" "bpython"))
        (switch-to-buffer "*bpython*"))))

  ;;; (Packages) ;;;
  (use-package lsp-pyright
    :if (executable-find "pyright-langserver"))
  (use-package anaconda-mode)
  (use-package py-autopep8)
  (use-package pylookup
    :quelpa (pylookup
             :fetcher github
             :repo "tsgates/pylookup"))
  (use-package python-environment
    :config
    (validate-setq
     ;; Locate of Python environment store.
     python-environment-directory (path-join *user-cache-directory*
                                             "python-environment")))
  (use-package lsp-jedi
    :config
    (with-eval-after-load "lsp-mode"
      (add-to-list 'lsp-disabled-clients 'pyls)
      (add-to-list 'lsp-enabled-clients 'jedi)))

  (use-package pipenv
    :if (executable-find "pipenv")
    :hook (python-mode-hook . pipenv-mode))

  (use-package flycheck-pycheckers
    :after flycheck
    :hook (flycheck-mode-hook . flycheck-pycheckers-setup))
  (use-package flycheck-mypy
    :if (executable-find "mypy"))
  (use-package flycheck-prospector
    :if (executable-find "prospector")
    :hook (flycheck-mode-hook . flycheck-prospector-setup))

  (use-package helm-pydoc
    :pin "MELPA"))


(provide 'modes/python)
;;; python.el ends here
