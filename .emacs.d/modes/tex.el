;;; tex.el --- TeX mode support
;;; Commentary:
;;; Code:

(defun user/tex-mode-hook ()
  "TeX mode hook."
  (turn-on-reftex)
  (bibtex-mode t))


(defun user/latex-mode-hook ()
  "LaTeX mode hook."
  (latex-math-mode t)
  (when (el-get-package-is-installed 'auto-complete-latex)
    (ac-l-setup))
  (when (el-get-package-is-installed 'ac-math)
    (add-ac-sources 'ac-source-math-unicode 'ac-source-math-latex
                    'ac-source-latex-commands))
  (when (el-get-package-is-installed 'auto-complete-auctex)
    (ac-auctex-setup)))


(defun user/auctex-init ()
  "Initialize AUCTeX."
  (setq-default
   ;; Make reftex interact with AucTeX.
   reftex-plug-into-AUCTeX t
   ;; Use PDF rather than DVI by default.
   TeX-PDF-mode t
   TeX-fold-mode t
   ;; Parse file after load/save unless it has a style hook.
   TeX-parse-self t
   TeX-auto-save t

   ;; (RefTeX) ;;
   ;; Prompt for optional arguments.
   reftex-cite-prompt-optional-args t))


(defun user/zotelo-init ()
  "Initialize Zotelo."
  (add-hook 'TeX-mode-hook 'zotelo-minor-mode))


(defun user/auto-complete-latex ()
  "Initialize LaTeX auto completion."
  (setq-default
   ac-l-dict-directory (path-join (el-get-package-directory
                                   'auto-complete-latex) "ac-l-dict"))
  (add-ac-modes 'latex-mode 'LaTeX-mode))


(defun user/ac-math-init ()
  "Initialize math auto completion."
  (setq-default
   ;; Enable unicode math input.
   ac-math-unicode-in-math-p t)
  (add-ac-modes 'latex-mode 'LaTeX-mode))


(defun user/tex-mode-init ()
  "Initialize generic text editing mode."
  (setq-default
   ;; (BibTeX) ;;
   bibtex-autokey-name-case-convert 'identity
   bibtex-autokey-year-length 4)

  (require-package '(:name auctex :after (user/auctex-init)))
  (require-package '(:name zotelo :after (user/zotelo-init)))
  (require-package '(:name ac-math :after (user/ac-math-init)))
  (require-package '(:name auto-complete-latex :after (user/auto-complete-latex)))
  (require-package '(:name auto-complete-auctex
                           :type github
                           :pkgname "monsanto/auto-complete-auctex"
                           :prepare (autoload 'ac-auctex-setup "auto-complete-auctex")))

  (add-hook 'TeX-mode-hook 'user/tex-mode-hook)
  (add-hook 'LaTeX-mode-hook 'user/latex-mode-hook)
  (add-hook 'latex-mode-hook 'user/latex-mode-hook))


(user/tex-mode-init)


(provide 'modes/text)
;;; text.el ends here
