;; (Code Conventions) ;;

;; Set the default C/C++ code style
(setq c-default-style "K&R")
(setq c++-default-style "Stroustrup")


;; Load the Google C/C++ style
(require 'google-c-style)


;; Enable automatic detection of indentation style
(setq load-path (cons "~/.emacs.d/vendor/dtrt-indent" load-path))
(add-hook 'c-mode-common-hook
  (lambda()
    (require 'dtrt-indent)
    (dtrt-indent-mode t)))

(add-hook 'c++-mode-common-hook
  (lambda()
    (require 'dtrt-indent)
    (dtrt-indent-mode t)))
