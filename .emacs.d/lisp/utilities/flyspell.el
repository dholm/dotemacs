;;; flyspell.el --- spell checking on the fly
;;; Commentary:
;;; Code:

(defun user/flyspell-mode-hook ()
  "Hook for fly spell mode."
  (after-load 'diminish
    (diminish 'flyspell-mode))

  ;;; (Bindings) ;;;
  (user/bind-key-local :code :spellcheck-word 'ispell-word)
  (user/bind-key-local :code :spellcheck-add-word 'user/flyspell-add-word-to-dict))


(defun user/flyspell-init ()
  "Initialize fly spell."
  (setq-default
   ;; Be silent when checking words.
   flyspell-issue-message-flag nil)

  (with-executable 'aspell
    (setq-default
     ispell-program-name "aspell"
     ispell-list-command "--list"
     ispell-extra-args '("--sug-mode=ultra")))

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

(when (or (executable-find "ispell")
         (executable-find "aspell"))
  (require-package '(:name flyspell :after (user/flyspell-init)))
  (require-package '(:name auto-dictionary)))


(provide 'utilities/flyspell)
;;; flyspell.el ends here
