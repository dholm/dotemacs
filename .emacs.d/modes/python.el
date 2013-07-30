;;; python --- initializes Python modes
;;; Commentary:
;;; Code:

(defun dholm/python-mode-hook ()
  "Python mode hook."
  (unless (derived-mode-p 'prog-mode)
    (run-hooks 'prog-mode-hook))
  ;; Configure python-mode
  (setq py-shell-name "ipython"
        py-load-pymacs-p t)
  ;; Load CEDET
  (dholm/python-mode-cedet-hook)
  ;; Load ropemacs
  (pymacs-load "ropemacs" "rope-")
  (setq ropemacs-enable-autoimport t)
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
       (append ac-sources '(ac-source-ropemacs))))


(defun dholm/python-mode-cedet-hook ()
  "CEDET hook for Python mode."
  (dholm/cedet-hook)
  (require 'semantic/wisent/python))


(defun dholm/jedi-init ()
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


(defun dholm/python-mode-init ()
  "Initialize Python mode."
  (add-hook 'python-mode-hook 'dholm/python-mode-hook))

(require-package '(:name jedi :after (dholm/jedi-init)))
(require-package '(:name python-mode :after (dholm/python-mode-init)))
(require-package '(:name pylookup))


(provide 'modes/python)
;;; python.el ends here
