;;; (Initialization) ;;;
(defconst *has-w3m* (executable-find "w3m"))

(when *has-w3m*
  (require-package (:name emacs-w3m :after (dholm/w3m-init))))

(setq w3m-use-cookies t
      w3m-coding-system 'utf-8
      w3m-file-coding-system 'utf-8
      w3m-file-name-coding-system 'utf-8
      w3m-input-coding-system 'utf-8
      w3m-output-coding-system 'utf-8
      w3m-terminal-coding-system 'utf-8)

(defun dholm/w3m-init ()
  (setq browse-url-browser-function 'w3m-browse-url)
  (autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
  (global-set-key (kbd "C-x m") 'browse-url-at-point))


(provide 'utilities/w3m)
