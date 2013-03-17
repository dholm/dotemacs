;; JavaScript mode

(push "~/.emacs.d/modes/js2-mode" load-path)
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook
          (lambda ()
	    (setq js2-use-font-lock-faces t)
	    (setq js2-indent-on-enter-key t)
	    (setq js2-basic-offset 2)
	    (set (make-local-variable 'ac-auto-start) 3)
	    (set (make-local-variable 'ac-auto-show-menu) t)))
