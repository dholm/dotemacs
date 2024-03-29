;;; shell.el --- initializes shell modes -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user--sh-mode-hook ()
  "Initialize mode for shell script editing."
  (validate-setq
   ;; Indent with four spaces.
   sh-basic-offset 4
   sh-indentation 4))


(defun user--shell-mode-common-hook ()
  "Shell mode common hook."
  (with-feature 'ansi-color
    ;; Enable ANSI colors for comint.
    (ansi-color-for-comint-mode-on))

  (with-feature 'shelldoc
    (shelldoc-minor-mode-on)))


(defun user--shell-mode-hook ()
  "Initialize mode for interactive shell."
  (user--shell-mode-common-hook)

  (validate-setq
   ;; Set up to use Bash with input echoing.
   explicit-shell-file-name "bash"
   explicit-bash-args '("-c" "export EMACS=; stty echo; bash")
   comint-process-echoes t))

(use-package shell
  :defer
  :hook ((sh-mode-hook . user--sh-mode-hook)
         (shell-mode-hook . user--shell-mode-hook))
  :config
  ;;; (Packages) ;;;
  (use-package bash-completion)
  (use-package company-shell
    :after (company)
    :config
    (add-to-list 'company-backends '(company-shell company-shell-env)))
  (use-package shelldoc)
  (use-package flycheck-checkbashisms
    :if (executable-find "checkbashisms")
    :config
    (flycheck-checkbashisms-setup))
  (use-package flycheck-bashate
    :if (executable-find "bashate")
    :config
    (flycheck-bashate-setup)))


(provide 'modes/shell)
;;; shell.el ends here
