;; (Code Conventions) ;;

(setq jedi:setup-keys t
      jedi:complete-on-dot t)

;; Enable installed helpers for Python
(defun dholm/python-mode-hook ()
  ;; Load CEDET
  (dholm/python-mode-cedet-hook)
  ;; Load jedi
  (jedi:setup)
  ;; Run spell-checker on strings and comments
  (flyspell-prog-mode)
  ;; Separate camel-case into separate words
  (subword-mode t)
  ;; ElDoc shows function documentation as you type
  (eldoc-mode t)
  ;; Show trailing whitespace
  (setq show-trailing-whitespace t)
  ;; Auto-completion sources
  (set (make-local-variable 'ac-sources)
       (append ac-sources '(ac-source-ropemacs)))
  ;; Keybindings
  (local-set-key (kbd "C-c h") 'pylookup-lookup)
  ;; Before save hook
  (add-hook 'before-save-hook
            ;; Delete trailing whitespace on save
            'delete-trailing-whitespace nil t))

(add-hook 'python-mode-hook 'dholm/python-mode-hook)

;; (Utilities) ;;


(provide 'modes/python)
