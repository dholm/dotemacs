;;; python.el --- initializes Python modes
;;; Commentary:
;;; Code:

(defconst *has-python* (executable-find "python"))


(defun user/python-mode-hook ()
  "Python mode hook."
  (unless (derived-mode-p 'prog-mode)
    (run-hooks 'prog-mode-hook))

  ;; Configure python-mode
  (setq-default
   py-shell-name "ipython"
   py-load-pymacs-p t)

  ;; Load CEDET
  (user/python-mode-cedet-hook)

  ;; Load ropemacs
  (pymacs-load "ropemacs" "rope-")
  (setq-default
   ropemacs-enable-autoimport t)

  ;; Load jedi
  (jedi:setup)

  ;; Separate camel-case into separate words
  (subword-mode t)

  ;; ElDoc shows function documentation as you type
  (eldoc-mode t)

  ;; Bind electric backspace to del which translates to backspace in
  ;; terminals.
  (define-key python-mode-map (kbd "DEL") 'py-electric-backspace)

  ;; Auto-completion sources
  (set (make-local-variable 'ac-sources)
       (append ac-sources '(ac-source-ropemacs)))

  ;; Register file types with find-file-in-project
  (after-load 'find-file-in-project
    (user/ffip-local-patterns "*.py")))


(defun user/python-mode-cedet-hook ()
  "CEDET hook for Python mode."
  (user/cedet-hook)
  (require 'semantic/wisent/python))


(defun user/jedi-init ()
  "Initialize jedi."
  ;;; (Faces) ;;;
  (after-load 'solarized-theme
    (solarized-with-values
      (eval
       `(custom-theme-set-faces
         'solarized
         '(jedi:highlight-function-argument ((t (:inherit bold))))))))

  (setq-default
   jedi:setup-keys t
   jedi:complete-on-dot t))


(defun user/python-mode-init ()
  "Initialize Python mode."
  (require-package '(:name jedi :after (user/jedi-init)))
  (require-package '(:name pylookup))

  (add-hook 'python-mode-hook 'user/python-mode-hook))

(when *has-python*
  (require-package '(:name python-mode :after (user/python-mode-init))))


(provide 'modes/python)
;;; python.el ends here
