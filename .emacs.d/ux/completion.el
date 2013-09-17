;;; completion.el --- Configures automatic code completion
;;; Commentary:
;;; Code:

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
  (require-package '(:name auto-complete :after (user/auto-complete-init)))
  (require-package '(:name tabkey2
                           :type http
                           :url "http://marmalade-repo.org/packages/tabkey2-1.40.el"
                           :build '(("mv" "tabkey2-1.40.el" "tabkey2.el"))
                           :compile "tabkey2.el"
                           :autoloads "tabkey2"
                           :after (user/tabkey2-init))))


(defun user/auto-complete-init ()
  "Initialize auto-complete."
  (require 'auto-complete-config)

  ;; Load default configuration
  (ac-config-default)

  (setq-default
   ;; Limit the number of candidates
   ac-candidate-limit 200
   ;; Do not automatically start completion
   ac-auto-start nil
   ;; Use fuzzy matching
   ac-use-fuzzy 1.5
   ;; Do not show menu unless requested
   ac-auto-show-menu nil
   ;; Allow normal navigation keys in menu
   ac-use-menu-map t
   ;; Do not auto-expand common candidates
   ac-expand-on-auto-complete nil
   ;; Show quick help popup after half a second
   ac-use-quick-help t
   ac-quick-help-delay 0.5
   ;; Store the completion history in the cache directory
   ac-comphist-file (path-join *user-cache-directory* "ac-comphist.dat"))

  (add-to-list 'ac-dictionary-directories (path-join *user-el-get-directory* "auto-complete" "ac-dict"))

  ;; Install workaround for Flyspell
  (add-hook 'flymake-mode-hook 'ac-flyspell-workaround)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)

  ;;; (Faces) ;;;
  (after-load 'solarized-theme
    (solarized-with-values
      (eval
       `(custom-theme-set-faces
         'solarized
         '(ac-candidate-face ((t (:foreground ,orange :background ,solarized-hl))))
         '(ac-selection-face ((t (:foreground ,cyan-hc :background ,cyan-lc))))
         '(ac-candidate-mouse-face ((t (:foreground ,cyan-lc :background ,cyan-hc))))
         '(ac-completion-face ((t (:foreground ,solarized-emph :underline t))))
         '(ac-gtags-candidate-face ((t (:foreground ,blue :background ,solarized-hl))))
         '(ac-gtags-selection-face ((t (:foreground ,blue-hc :background ,blue-lc))))
         '(ac-yasnippet-candidate-face ((t (:foreground ,yellow :background ,solarized-hl))))
         '(ac-yasnippet-selection-face ((t (:foreground ,yellow-hc :background ,yellow-lc))))))))

  ;; Enable auto-complete globally
  (global-auto-complete-mode t)
  (after-load 'diminish
    (diminish 'auto-complete-mode))

  ;;; (Bindings) ;;;
  (ac-set-trigger-key (kbd "TAB"))
  (define-key user/code-map (kbd "SPC") 'auto-complete))


(defun user/tabkey2-init ()
  "Initialize tabkey2."
  ;; Just load it since it is used by prog-mode
  (require 'tabkey2))


(user/completion-init)


(provide 'ux/completion)
;;; completion.el ends here
