;;; flyspell.el --- spell checking on the fly
;;; Commentary:
;;; Code:

(defconst *user-auto-dictionary-buffer-limit* (* 128 1024)
  "Maximum buffer size for automatic dictionary detection.")


(defun user/flyspell-mode-common-hook ()
  "Hook for fly spell common mode."
  (if (> (buffer-size) *user-auto-dictionary-buffer-limit*)
      ;; Disables automatic dictionary detection in large buffers.
      (auto-dictionary-mode -1)
    (with-feature 'flyspell-lazy
      ;; Enables flyspell lazy mode for small buffers.
      (flyspell-lazy-mode t)))

  (after-load 'diminish
    (diminish 'flyspell-mode))

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


(defun user/rw-hunspell-init ()
  "Initialize rw-hunspell."
  (after-load 'ispell
    (when ispell-really-hunspell
      ;; Initialize `rw-hunspell` if Hunspell is in use.
      (rw-hunspell-setup))))


(defun user/flyspell-init ()
  "Initialize fly spell."
  (setq-default
   ;; Be silent when checking words.
   flyspell-issue-message-flag nil)

  (cond
   (;; Disable Hunspell due to issues on some machines.
    (and nil (executable-find "hunspell"))
    (setq-default
     ispell-program-name "hunspell"
     ispell-really-hunspell t
     ispell-extra-args '("-a" "-i" "utf-8")))
   ((executable-find "aspell")
    (setq-default
     ispell-program-name "aspell"
     ispell-really-aspell t
     ispell-list-command "--list"
     ;; Improve performance by reducing suggestions.
     ispell-extra-args '("--sug-mode=ultra" "--dont-suggest"))))

  (add-hook 'flyspell-mode-hook 'user/flyspell-mode-hook)
  (add-hook 'flyspell-prog-mode-hook 'user/flyspell-prog-mode-hook))

(when (or (executable-find "ispell")
          (executable-find "aspell")
          (executable-find "hunspell"))
  (req-package flyspell
    :loader :el-get
    :config (user/flyspell-init))
  (req-package flyspell-lazy
    :config (user/flyspell-lazy-init))
  (req-package auto-dictionary)
  (with-executable 'hunspell
    (req-package rw-hunspell
      :config (user/rw-hunspell-init))))


(provide 'utilities/flyspell)
;;; flyspell.el ends here
