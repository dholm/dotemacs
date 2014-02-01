;;; completion.el --- Configures automatic code completion
;;; Commentary:
;;; Code:

(defun user/auto-complete-init ()
  "Initialize auto-complete."
  (require 'auto-complete-config)

  ;; Load default configuration.
  (ac-config-default)
  (after-load 'diminish
    (diminish 'auto-complete-mode))

  (setq-default
   ;; Limit the number of candidates.
   ac-candidate-limit 20
   ;; Wait five seconds until showing completions.
   ac-delay 5
   ;; Automatically start completion after two characters.
   ac-auto-start 2
   ;; Use fuzzy matching.
   ac-use-fuzzy 1.5
   ;; Automatically show menu after 400ms.
   ac-auto-show-menu 0.4
   ;; Allow normal navigation keys in menu.
   ac-use-menu-map t
   ;; Do not auto-expand common candidates.
   ac-expand-on-auto-complete nil
   ;; Show quick help popup after half a second.
   ac-use-quick-help t
   ac-quick-help-delay 0.5
   ;; Store the completion history in the cache directory.
   ac-comphist-file (path-join *user-cache-directory* "ac-comphist.dat"))

  (add-to-list 'ac-dictionary-directories
               (path-join *user-el-get-directory* "auto-complete" "ac-dict"))

  ;; Install workaround for Flyspell
  (add-hook 'flymake-mode-hook 'ac-flyspell-workaround)

  ;; Set up auto-complete for lisp-interaction- and ielm-mode.
  (dolist (hook (list 'lisp-interaction-mode-hook
                      'ielm-mode-hook))
    (add-hook hook 'ac-emacs-lisp-mode-setup))

  ;;; (Bindings) ;;;
  (ac-set-trigger-key (user/get-key :code :try-complete))
  ;; Return should complete item in menu.
  (define-key ac-completing-map (kbd "C-m") nil)
  (define-key ac-menu-map (kbd "C-m") 'ac-complete)
  (user/bind-key-global :code :complete 'auto-complete))


(defun user/completion-init ()
  "Initialize automatic code completion."
  (setq-default
   ;; Use complete when auto-complete is unavailable
   tab-always-indent 'complete)

  (add-to-list 'completion-styles 'initials t)

  ;;; (Functions) ;;;
  (defun add-ac-sources (&rest sources)
    "Add SOURCES for auto-complete after it has been loaded."
    (after-load 'auto-complete
      (dolist (source sources)
        (if (boundp 'ac-sources)
            (add-to-list 'ac-sources source)
          (error "Declaration of ac-sources is missing!")))))

  (defun add-ac-modes (&rest major-modes)
    "Add MAJOR-MODES for auto-complete after it has been loaded."
    (after-load 'auto-complete
      (dolist (mode major-modes)
        (if (boundp 'ac-modes)
            (add-to-list 'ac-modes mode)
          (error "Declaration of ac-modes is missing!")))))

  ;;; (Packages) ;;;
  (require-package '(:name auto-complete :after (user/auto-complete-init))))

(user/completion-init)


(provide 'ux/completion)
;;; completion.el ends here
