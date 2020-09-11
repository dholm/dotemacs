;;; flyspell.el --- spell checking on the fly -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defconst *user-auto-dictionary-buffer-limit* (* 128 1024)
  "Maximum buffer size for automatic dictionary detection.")


(defun user--flyspell-mode-common-hook ()
  "Hook for fly spell common mode."
  (if (> (buffer-size) *user-auto-dictionary-buffer-limit*)
      ;; Disables automatic dictionary detection in large buffers.
      (guess-language-mode -1)
    (with-feature 'flyspell-lazy
      (guess-language-mode t)
      ;; Enables flyspell lazy mode for small buffers.
      (flyspell-lazy-mode t)))

  (with-feature 'auto-correct
    (auto-correct-mode t))

  ;;; (Bindings) ;;;
  (user/bind-key-local :code :spellcheck-word 'ispell-word)
  (user/bind-key-local :code :spellcheck-add-word 'user/flyspell-add-word-to-dict))


(defun user--flyspell-mode-hook ()
  "Hook for fly spell mode."
  (user--flyspell-mode-common-hook))


(defun user--flyspell-prog-mode-hook ()
  "Hook for fly spell prog mode."
  (user--flyspell-mode-common-hook))


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

(defun user-flyspell-p ()
  "Check if flyspell is available."
  (or (executable-find "ispell")
      (executable-find "aspell")
      (executable-find "hunspell"))
  nil)

(when (user-flyspell-p)
  (use-package flyspell
    :ensure nil
    :diminish flyspell-mode
    :hook ((flyspell-mode-hook . user--flyspell-mode-hook)
           (flyspell-prog-mode-hook . user--flyspell-prog-mode-hook))
    :config
    (validate-setq
     ;; Be silent when checking words.
     flyspell-issue-message-flag nil)

    (cond
     (;; Disable Hunspell due to issues on some machines.
      (and nil (executable-find "hunspell"))
      (validate-setq
       ispell-program-name "hunspell"
       ispell-really-hunspell t
       ispell-extra-args '("-a" "-i" "utf-8")))
     ((executable-find "aspell")
      (progn
        (validate-setq
         ispell-program-name "aspell"
         ispell-really-aspell t
         ;; Improve performance by reducing suggestions.
         ispell-extra-args '("--sug-mode=ultra" "--dont-suggest"))
        (when (boundp 'flyspell-list-command)
          (validate-setq
           flyspell-list-command "--list")))))

    (use-package auto-correct
      :diminish auto-correct-mode)

    (use-package helm-flyspell
      :bind ("C-c c s" . helm-flyspell-correct))

    (use-package flyspell-lazy
      :config
      (validate-setq
       ;; Idle timeout before running spell check on region.
       flyspell-lazy-idle-seconds 10
       ;; Idle timeout before running spell check on entire buffer.
       flyspell-lazy-window-idle-seconds 60))

    (use-package guess-language
      :diminish guess-language-mode
      :config
      (validate-setq
       ;; By default only guess English.
       guess-language-languages '(en)))

    (use-package rw-hunspell
      :if (executable-find "hunspell")
      :config
      (with-eval-after-load 'ispell
        (when ispell-really-hunspell
          ;; Initialize `rw-hunspell` if Hunspell is in use.
          (rw-hunspell-setup))))))


(provide 'utilities/flyspell)
;;; flyspell.el ends here
