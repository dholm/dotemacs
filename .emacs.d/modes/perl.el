;; (Utilities) ;;

(defun pde-perl-mode-hook ()
  (local-set-key (kbd "C-c s") 'compile-dwim-compile)
  (local-set-key (kbd "C-c r") 'compile-dwim-run)
  (setq compilation-buffer-name-function 'pde-compilation-buffer-name)

  ;; chmod when saving
  (when (and buffer-file-name
             (not (string-match "\\.\\(pm\\|pod\\)$" (buffer-file-name))))
    (add-hook 'after-save-hook 'executable-chmod nil t))
  (set (make-local-variable 'compile-dwim-check-tools)
       nil))

(defun dholm/perl-mode-hook ()
  (pde-perl-mode-hook)
  ;; Run spell-checker on strings and comments
  (flyspell-prog-mode))

(add-hook 'perl-mode-hook 'dholm/perl-mode-hook)


(provide 'modes/perl)
