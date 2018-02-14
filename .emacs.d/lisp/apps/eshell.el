;;; eshell.el --- Initialize the Emacs shell -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defconst *shell-cache-directory* (path-join *user-cache-directory*
                                             "shell"))


(defun user--eshell-mode-hook ()
  "Mode hook for eshell."
  (user--shell-mode-common-hook)

  (when (feature-p 'eshell-bookmark)
    (eshell-bookmark-setup))

  (turn-on-eldoc-mode)

  (when (user/auto-complete-p)
    ;; Use auto-complete for completion.
    (add-ac-sources 'ac-source-pcomplete))

  (when (user/company-mode-p)
    (company-mode t)

    (when (feature-p 'company-eshell-autosuggest)
      (validate-setq
       company-backends '(company-eshell-autosuggest)
       company-frontends '(company-preview-frontend))))

  (with-feature 'esh-autosuggest
    (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)))


(defun user/shorten-path (path)
  "Shorten the length of PATH."
  (let ((scount (1- (count ?/ path))))
    (dotimes (i scount)
      (string-match "\\(/\\.?.\\)[^/]+" path)
      (setq path (replace-match "\\1" nil nil path))))
  path)


(defun user/shell-prompt ()
  "Return a prompt for the shell."
  (concat
   (with-face (concat (user/shorten-path (eshell/pwd)) " ")
              :inherit 'header-line)
   (with-face (format-time-string "(%Y-%m-%d %H:%M) " (current-time))
              :inherit 'header-line)
   (with-face
    (or (ignore-errors
          (format "(%s)" (vc-responsible-backend default-directory))) "")
    :inherit 'header-line)
   (with-face "\n" :inherit 'header-line)
   (with-face user-login-name 'font-lock-variable-name-face)
   "@"
   (with-face "localhost" 'font-lock-keyword-face)
   (if (= (user-uid) 0)
       (with-face " #" 'font-lock-warning-face)
     " $")
   " "))


(defun user/raise-eshell ()
  "Start, or switch to, `eshell' in the current working directory."
  (interactive)
  (let ((path (file-name-directory
               (or (buffer-file-name) *user-home-directory*)))
        (hasfile (not (eq (buffer-file-name) nil))))
    (eshell)
    (if (and hasfile (eq eshell-process-list nil))
        (progn
          (eshell/cd path)
          (eshell-reset)))))


(use-package eshell
  :commands user/raise-eshell
  :defer
  :hook (eshell-mode-hook . user--eshell-mode-hook)
  :init
  (user/bind-key-global :apps :shell 'user/raise-eshell)
  :config
  (validate-setq
   ;; Set the path to the shell cache store.
   eshell-directory-name *shell-cache-directory*)

  (use-package esh-module
    :ensure nil
    :config
    (add-many-to-list
     'eshell-modules-list
     ;; Rebind keys while point is in a region of input text.
     'eshell-rebind
     ;; Smart command output management.
     'eshell-smart
     ;; Extra alias functions.
     'eshell-xtra))

  (when (user/auto-complete-p)
    ;; For `ac-source-pcomplete'.
    (add-ac-modes 'eshell-mode))

  (use-package em-script
    :ensure nil
    :config
    (validate-setq
     ;; And the shell login script.
     eshell-login-script (path-join *user-home-directory* ".eshellrc")))

  (use-package em-term
    :ensure nil
    :config
    (add-many-to-list
     ;; Commands that should be run using term for better handling of
     ;; ANSI control codes.
     'eshell-visual-commands
     "htop" "perf" "ssh" "telnet" "tmux"))

  (use-package em-prompt
    :ensure nil
    :config
    (validate-setq
     ;; Set eshell prompt.
     eshell-prompt-function 'user/shell-prompt
     eshell-highlight-prompt nil
     eshell-prompt-regexp "^[^#$\n]*[#$] "))
  (use-package em-hist
    :ensure nil
    :config
    (validate-setq
     ;; Set a decent history size.
     eshell-history-size 10000
     eshell-save-history-on-exit t))
  (use-package em-term
    :ensure nil
    :config
    (validate-setq
     ;; Announce the terminal type.
     eshell-term-name "eterm-color"))

  (use-package eshell-bookmark)

  (use-package company-eshell-autosuggest)

  (use-package esh-autosuggest
    :hook (eshell-mode-hook . esh-autosuggest-mode)))

(use-package helm-shell
  :ensure helm
  :defer
  :hook ((eshell-mode-hook
          . (lambda ()
              (bind-key "C-c C-l"
                        #'helm-eshell-history
                        eshell-mode-map)))
         (eshell-mode-hook
          . (lambda ()
              (bind-key [remap eshell-pcomplete]
                        'helm-esh-pcomplete eshell-mode-map))))
  :config
  (bind-key "C-c C-l" #'helm-comint-input-ring shell-mode-map))


(provide 'apps/eshell)
;;; eshell.el ends here
