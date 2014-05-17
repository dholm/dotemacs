;;; flyspell.el --- spell checking on the fly
;;; Commentary:
;;; Code:

(defun user/flyspell-mode-common-hook ()
  "Hook for fly spell common mode."
  (after-load 'diminish
    (diminish 'flyspell-mode))

  (with-feature 'flyspell-lazy
    (flyspell-lazy-mode t))

  ;;; (Bindings) ;;;
  (user/bind-key-local :code :spellcheck-word 'ispell-word)
  (user/bind-key-local :code :spellcheck-add-word 'user/flyspell-add-word-to-dict))


(defun user/flyspell-mode-hook ()
  "Hook for fly spell mode."
  (user/flyspell-mode-common-hook))


(defun user/flyspell-prog-mode-hook ()
  "Hook for fly spell prog mode."
  (user/flyspell-mode-common-hook))


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
    (ispell-pdict-save t)))


(defun user/flyspell-lazy-init ()
  "Initialize fly spell lazy."
  (setq-default
   ;; Idle timeout before running spell check on region.
   flyspell-lazy-idle-seconds 10
   ;; Idle timeout before running spell check on entire buffer.
   flyspell-lazy-window-idle-seconds 60))


(defun user/flyspell-init ()
  "Initialize fly spell."
  (setq-default
   ;; Be silent when checking words.
   flyspell-issue-message-flag nil)

  (with-executable 'aspell
    (setq-default
     ispell-program-name "aspell"
     ispell-list-command "--list"
     ;; Improve performance by reducing suggestions.
     ispell-extra-args '("--sug-mode=ultra" "--dont-suggest")))

  (add-hook 'flyspell-mode-hook 'user/flyspell-mode-hook)
  (add-hook 'flyspell-prog-mode-hook 'user/flyspell-prog-mode-hook))

(when (or (executable-find "ispell")
          (executable-find "aspell"))
  (require-package '(:name flyspell :after (user/flyspell-init)))
  (require-package '(:name flyspell-lazy :after (user/flyspell-lazy-init)))
  (require-package '(:name auto-dictionary)))


(provide 'utilities/flyspell)
;;; flyspell.el ends here
