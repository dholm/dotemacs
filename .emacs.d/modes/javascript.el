;; JavaScript mode

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))


(defun dholm/javascript-mode-hook ()
  ;; Load CEDET
  (dholm/javascript-mode-cedet-hook)
  ;; Configure js2-mode
  (setq js2-use-font-lock-faces t
        js2-indent-on-enter-key t
        js2-basic-offset 2)
  ;; Configure autocompletion
  (set (make-local-variable 'ac-auto-start) 3
       (make-local-variable 'ac-auto-show-menu) t)
  ;; Run spell-checker on strings and comments
  (flyspell-prog-mode))

(add-hook 'javascript-mode-hook 'dholm/javascript-mode-hook)


(provide 'modes/javascript)
