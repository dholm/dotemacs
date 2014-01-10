;;; flyspell.el --- spell checking on the fly
;;; Commentary:
;;; Code:

(defun user/flyspell-mode-hook ()
  "Hook for fly spell mode."
  ;;; (Bindings) ;;;
  (define-key user/code-map (kbd "s") 'ispell-word)
  (define-key user/code-map (kbd "S") 'user/flyspell-add-word-to-dict))


(defun user/flyspell-lazy-init ()
  "Initialize fly spell lazy."
  (flyspell-lazy-mode t))


(defun user/flyspell-init ()
  "Initialize fly spell."
  (when *has-aspell*
    (setq-default
     ispell-program-name "aspell"
     ispell-list-command "--list"
     ispell-extra-args '("--sug-mode=ultra")))

  (require-package '(:name flyspell-lazy
                           :type github
                           :pkgname "rolandwalker/flyspell-lazy"
                           :features (flyspell-lazy)
                           :after (user/flyspell-lazy-init)))
  (require-package '(:name auto-dictionary))

  (add-hook 'flyspell-mode-hook 'user/flyspell-mode-hook)

  ;;; (Functions) ;;;
  (defun user/flyspell-add-word-to-dict ()
    "Add the word at the current location to the private dictionary without question."
    (interactive)
    ;; Use the correct dictionary.
    (flyspell-accept-buffer-local-defs)
    (setq opoint (point-marker))
    (let ((cursor-location (point))
          (word (flyspell-get-word nil)))
      (if (consp word)
          (let ((start (car (cdr word)))
                (end (car (cdr (cdr word))))
                (word (car word)))
            ;; The word is incorrect, we have to propose a replacement.
            (flyspell-do-correct 'save nil word cursor-location start end opoint)))
      (ispell-pdict-save t))))

(when (or *has-ispell* *has-aspell*)
  (user/flyspell-init))


(provide 'utilities/flyspell)
;;; flyspell.el ends here
