;; (Code Conventions) ;;

;; Enable installed helpers for Python
(defun dholm/python-mode-hook ()
  ;; Run spell-checker on strings and comments
  (flyspell-prog-mode)
  (flymake-mode)
  ;; Separate camel-case into separate words
  (subword-mode t)
  ;; ElDoc shows function documentation as you type
  (eldoc-mode t)
  ;; Show trailing whitespace
  (setq show-trailing-whitespace t)
  ;; Auto-completion sources
  (set (make-local-variable 'ac-sources)
       (append ac-sources '(ac-source-ropemacs)))
  ;; Before save hook
  (add-hook 'before-save-hook
            ;; Delete trailing whitespace on save
            'delete-trailing-whitespace nil t))
(add-hook 'python-mode-hook 'dholm/python-mode-hook)


;; (Utilities) ;;

;; Run Python checkers when in flymake-mode
(when (load "flymake" t)
  (defun flymake-pycheckers-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "~/.emacs.d/bin/pycheckers"  (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pycheckers-init)))
