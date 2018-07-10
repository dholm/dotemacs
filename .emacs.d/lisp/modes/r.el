;;; r.el --- R mode support -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user--R-mode-hook ()
  "R programming mode hook."
  (unless (derived-mode-p 'prog-mode)
    (run-hooks 'prog-mode-hook)))


(defun user--ess-mode-hook ()
  "ESS mode hook."
  (user--ess-mode-common-hook))


(defun user--ess-mode-common-hook ()
  "ESS common mode hook."
  (when (feature-p 'ac-R)
    (ess-ac-init))

  ;;; (Bindings) ;;;
  (user/bind-key-local :code :eval-buffer 'ess-eval-buffer)
  (user/bind-key-local :code :eval-function 'ess-eval-function)
  (user/bind-key-local :code :eval-selection 'user/ess-eval-region)
  (user/bind-key-local :code :eval-expression 'ess-eval-line))


(defun user--inferior-ess-mode-hook ()
  "Inferior ESS mode hook."
  (user--ess-mode-common-hook))


(defun user--ess-R-post-run-hook ()
  "ESS R post run hook."
  ;; Make R expand to the full width of the buffer.
  (ess-execute-screen-options))


(defun user/ess-start-R ()
  "If R is not running launch it in an inferior frame."
  (interactive)
  (if (not (member "*R*" (mapcar (function buffer-name) (buffer-list))))
      (progn
        (delete-other-windows)
        (let ((window1 (selected-window))
              (window1-name (buffer-name))
              (window2 (split-window (selected-window))))
          (R)
          (set-window-buffer window2 "*R*")
          (set-window-buffer window1 window1-name)))))


(defun user/ess-eval-region ()
  "Evaluate active region."
  (interactive)
  (user/ess-start-R)
  (if (and transient-mark-mode mark-active)
      (call-interactively 'ess-eval-line-and-step)))

(use-package ess
  :if (executable-find "R")
  :defer
  :mode (("\.R$" . R-mode)
         ("\.Rd$" . Rd-mode)
         ("\.Rnw$" . Rnw-mode))
  :bind (:map ess-mode-map
              ;; Workaround issue with ess yank.
              ([remap yank] . yank))
  :init
  (add-hook 'ess-mode-hook 'user--ess-mode-hook)
  (add-hook 'inferior-ess-mode-hook 'user--inferior-ess-mode-hook)
  (add-hook 'R-mode-hook 'user--R-mode-hook)
  (add-hook 'ess-R-post-run-hook 'user--ess-R-post-run-hook)
  (when (feature-p 'polymode)
    (add-auto-mode 'poly-noweb+r-mode "\\.Snw$" "\\.Rnw")
    (add-auto-mode 'poly-markdown+r-mode "\\.Rmd")
    (add-auto-mode 'poly-rapport-mode "\\.rapport")
    (add-auto-mode 'poly-html+r-mode "\\.Rhtml")
    (add-auto-mode 'poly-brew+r-mode "\\.Rbrew")
    (add-auto-mode 'poly-r+c++-mode "\\.Rcpp")
    (add-auto-mode 'poly-c++r-mode "\\.cppR"))

  (user/bind-key-global :apps :statistics 'R)
  :config
  (validate-setq
   ;; The default ESS dialect is R.
   ess-dialect "R"
   ;; Use ElDoc in all ESS modes.
   ess-use-eldoc t
   ess-eldoc-show-on-symbol t
   ;; Optimize by only printing results, not code.
   ess-eval-visibly-p nil
   ;; Start R in the current directory.
   ess-ask-for-ess-directory nil
   ess-local-process-name "R"
   ;; Enable ElDoc support.
   ess-use-eldoc t)

  (cond
   ((user/auto-complete-p)
    (validate-setq
     ;; Use auto completion in ESS modes.
     ess-use-auto-complete t))
   ((user/company-mode-p)
    (validate-setq
     ;; Use company mode completion in ESS modes.
     ess-use-company t)))

  (use-package ess-smart-underscore)
  (use-package ac-R
    :after (auto-complete)
    :init
    (autoload 'ess-ac-init "ac-R")))


(provide 'modes/r)
;;; r.el ends here
