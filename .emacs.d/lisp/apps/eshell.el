;;; eshell.el --- Initialize the Emacs shell
;;; Commentary:
;;; Code:

(defconst *shell-cache-directory* (path-join *user-cache-directory*
                                             "shell"))


(defun user/eshell-mode-hook ()
  "Mode hook for eshell."
  (user/shell-mode-common-hook)

  (turn-on-eldoc-mode)

  ;; Use auto-complete for completion.
  (add-ac-sources 'ac-source-pcomplete)
  (add-ac-modes 'eshell-mode))


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
   (with-face user-login-name :foreground "blue")
   "@"
   (with-face "localhost" :foreground "green")
   (if (= (user-uid) 0)
       (with-face " #" :foreground "red")
     " $")
   " "))


(defun user/eshell-init ()
  "Initialize the Emacs shell."
  (setq-default
   ;; Set the path to the shell cache store.
   eshell-directory-name *shell-cache-directory*
   ;; And the shell login script.
   eshell-login-script (path-join *user-home-directory* ".eshellrc")
   ;; Set eshell prompt.
   eshell-prompt-function 'user/shell-prompt
   eshell-highlight-prompt nil
   eshell-prompt-regexp "^[^#$\n]*[#$] "
   ;; Set a decent history size.
   eshell-history-size 10000
   eshell-save-history-on-exit t
   ;; Announce the terminal type.
   eshell-term-name "eterm-color"
   ;; Allow using buffer names directly in redirection.
   eshell-buffer-shorthand t)

  (after-load 'esh-module
    (add-many-to-list 'eshell-modules-list
                      ;; Rebind keys while point is in a region of input text.
                      'eshell-rebind
                      ;; Smart command output management.
                      'eshell-smart
                      ;; Extra alias functions.
                      'eshell-xtra))

  (after-load 'em-term
    ;; Commands that should be run using term for better handling of ANSI control
    ;; codes.
    (add-many-to-list 'eshell-visual-commands
                      "htop" "perf" "ssh" "telnet" "tmux"))

  (add-hook 'eshell-mode-hook 'user/eshell-mode-hook)

  ;;; (Bindings) ;;;
  (user/bind-key-global :apps :shell 'eshell)

  ;;; (Packages) ;;;
  (require-package '(:name eshell-manual)))

(user/eshell-init)


(provide 'apps/eshell)
;;; eshell.el ends here
