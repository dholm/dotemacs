(defun dholm/scala-mode-hook ()
  (ensime-scala-mode-hook)
  ;; Run spell-checker on strings and comments
  (flyspell-prog-mode)
  ;; Show trailing whitespace
  (setq show-trailing-whitespace t)
  ;; Before save hook
  (add-hook 'before-save-hook
            ;; Delete trailing whitespace on save
            'delete-trailing-whitespace nil t))

(add-hook 'scala-mode-hook 'dholm/scala-mode-hook)


(provide 'modes/scala)
