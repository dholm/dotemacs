;;; python --- initializes Python modes
;;; Commentary:
;;; Code:

(defun user/python-mode-hook ()
  "Python mode hook."
  (unless (derived-mode-p 'prog-mode)
    (run-hooks 'prog-mode-hook))
  ;; Configure python-mode
  (setq py-shell-name "ipython"
        py-load-pymacs-p t)
  ;; Load CEDET
  (user/python-mode-cedet-hook)
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
  (add-hook 'python-mode-hook 'user/python-mode-hook))

(require-package '(:name python-mode
                         :type bzr
                         :url "lp:python-mode"
                         :load-path ("." "test")
                         :compile nil
                         :prepare (progn
                                    (autoload 'python-mode "python-mode"
                                      "Python editing mode." t)
                                    (autoload 'doctest-mode "doctest-mode"
                                      "Doctest unittest editing mode." t)
                                    (setq py-install-directory
                                          (el-get-package-directory "python-mode"))
                                    (add-to-list 'auto-mode-alist
                                                 '("\\.py$" . python-mode))
                                    (add-to-list 'interpreter-mode-alist
                                                 '("python" . python-mode)))
                         :after (user/python-mode-init)))
(require-package '(:name jedi :after (user/jedi-init)))
(require-package '(:name pylookup))


(provide 'modes/python)
;;; python.el ends here
