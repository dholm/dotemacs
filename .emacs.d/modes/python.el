;;; python --- initializes Python modes
;;; Commentary:
;;; Code:

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


(defun dholm/python-mode-cedet-hook ()
  "CEDET hook for Python mode."
  (dholm/cedet-hook)
  (require 'semantic/wisent/python))


;; Enable installed helpers for Python
(defun dholm/python-mode-hook ()
  "Python mode hook."
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
  ;; Run spell-checker on strings and comments
  (flyspell-prog-mode)
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
  ;; Before save hook
  (add-hook 'before-save-hook
            ;; Delete trailing whitespace on save
            'delete-trailing-whitespace nil t))

(add-hook 'python-mode-hook 'dholm/python-mode-hook)


;; (Utilities) ;;
(require-package '(:name jedi :after (dholm/jedi-init)))
(require-package '(:name python-mode))
(require-package '(:name pylookup))


(provide 'modes/python)
;;; python.el ends here
