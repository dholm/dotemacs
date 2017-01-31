;;; tex.el --- TeX mode support
;;; Commentary:
;;; Code:

(defvar user/tex-preview-setup nil
  "Non-nil if preview has been set up.")


(defun user--tex-mode-hook ()
  "TeX mode hook."
  (user/gnu-global-enable)

  (when (and (display-graphic-p)
             (not user/tex-preview-setup))
    ;; Setup LaTeX preview.
    (if (feature-p 'sage-mode)
        (after-load 'sage
          ;; If Sage is available, it must be loaded first.
          (LaTeX-preview-setup))
      (LaTeX-preview-setup))
    (setq user/tex-preview-setup t))

  (turn-on-reftex)
  (outline-minor-mode t)
  (visual-line-mode t)
  (user/smartparens-enable)

  ;; Completion backends.
  (cond
   ((user/auto-complete-p)
    (with-feature 'ac-math
      (add-ac-modes 'latex-mode 'LaTeX-mode)))
   ((user/company-mode-p)
    (with-feature 'company-math
      (add-company-sources 'company-math-symbols-latex 'company-latex-commands))))

  (when (feature-p 'mic-paren)
    ;; Match context to quoted parentheses.
    (paren-toggle-matching-quoted-paren t)
    ;; Match paired delimiters.
    (paren-toggle-matching-paired-delimiter t))

  (when (feature-p 'mode-compile)
    ;; Override AUCTeX in favor of mode-compile.
    (kill-local-variable 'compile-command))

  ;;; (Bindings) ;;;
  (when (feature-p 'ltx-help)
    (user/bind-key-local :doc :reference 'latex-help))
  (user/bind-key-local :nav :functions/toc 'reftex-toc)
  (when (display-graphic-p)
    (user/bind-key-local :code :run 'preview-document))
  (when (feature-p 'ebib)
    (user/bind-key-local :nav :references 'ebib)))


(defun user--latex-mode-hook ()
  "LaTeX mode hook."
  ;; Enable TeX math macros.
  (LaTeX-math-mode t)

  (when (user/auto-complete-p)
    ;; Enable auto-complete.
    (with-feature 'ac-math
      (add-ac-sources 'ac-source-math-unicode 'ac-source-math-latex
                      'ac-source-latex-commands)))

  ;; Activate improved sentence filling.
  (ad-activate 'LaTeX-fill-region-as-paragraph))


(defun user--bibtex-mode-hook ()
  "BibTeX mode hook.")


(defun user--auctex-config ()
  "Initialize AUCTeX."
  (setq-default
   ;; Use synctex to communicate with LaTeX.
   TeX-source-correlate-method 'synctex
   LaTeX-command "latex -synctex=1 -shell-escape"
   ;; Do not ask about saving buffers before starting TeX.
   TeX-save-query nil
   ;; Use PDF rather than DVI by default.
   TeX-PDF-mode t
   TeX-fold-mode t
   TeX-interactive-mode t
   ;; Parse file after load/save unless it has a style hook.
   TeX-parse-self t
   TeX-auto-save t
   ;; Display help on error messages.
   TeX-display-help t
   ;; Automatically remove tabs.
   TeX-auto-untabify t
   ;; Support for backwards search in documents.
   TeX-source-correlate-mode t
   TeX-source-correlate-start-server t
   TeX-master nil

   ;; (RefTeX) ;;
   ;; Make reftex interact with AucTeX.
   reftex-plug-into-AUCTeX t
   ;; Prompt for optional arguments.
   reftex-cite-prompt-optional-args t
   ;; Try to guess the label type before prompting.
   reftex-guess-label-type t
   ;; Use nice fonts for toc.
   reftex-use-fonts t
   ;; Revisit files if necessary when browsing toc.
   reftex-revisit-to-follow t
   ;; Center on the section currently being edited.
   reftex-auto-recenter-toc t
   ;; Save parse info to avoid reparsing every time a file is visited.
   reftex-save-parse-info t
   ;; Cache selection buffers for faster access.
   reftex-use-multiple-selection-buffers t)

  (after-load 'preview
    ;; Support previewing of TikZ.
    (add-to-list 'preview-default-preamble "\\PreviewEnvironment{tikzpicture}" t))

  (after-load 'auctex
    (when (user/company-mode-p)
      (with-feature 'company-auctex
        ;; Enable company AUCTeX completion.
        (company-auctex-init))))

  (cond
   ((eq system-type 'darwin)
    (setq-default
     TeX-view-program-list (quote (("Preview" "open -a Preview.app %o")
                                   ("Skim" "open -a Skim.app %o")
                                   ("open" "open %o")))
     TeX-view-program-selection '((output-pdf "Preview"))))
   (t (with-executable 'evince
        (setq-default
         TeX-view-program-list '(("Evince" "evince --page-index=%(outpage) %o"))
         TeX-view-program-selection '((output-pdf "Evince"))))))

  ;;; (Functions) ;;;
  (defadvice LaTeX-fill-region-as-paragraph (around LaTeX-sentence-filling)
    "Start each sentence on a new line.

Makes it easier to version control LaTeX-files."
    (let ((from (ad-get-arg 0))
          (to-marker (set-marker (make-marker) (ad-get-arg 1)))
          tmp-end)
      (while (< from (marker-position to-marker))
        (forward-sentence)
        ;; might have gone beyond to-marker --- use whichever is smaller:
        (ad-set-arg 1 (setq tmp-end (min (point) (marker-position to-marker))))
        ad-do-it
        (ad-set-arg 0 (setq from (point)))
        (unless (or
                 (bolp)
                 (looking-at "\\s *$"))
          (LaTeX-newline)))
      (set-marker to-marker nil))))


(defun user--ebib-config ()
  "Initialize Ebib."
  (setq-default
   ebib-file-search-dirs
   '(lambda ()
      (with-project project (path-buffer-abs)
        (user/proj-root project)))))


(defun user--zotelo-config ()
  "Initialize Zotelo."
  (add-hook 'TeX-mode-hook 'zotelo-minor-mode))


(defun user--auto-complete-latex-config ()
  "Initialize LaTeX auto completion."
  (setq-default
   ac-l-dict-directory (path-join (el-get-package-directory
                                   'auto-complete-latex) "ac-l-dict"))
  (add-ac-modes 'latex-mode 'LaTeX-mode))


(defun user--ac-math-config ()
  "Initialize math auto completion."
  (setq-default
   ;; Enable unicode math input.
   ac-math-unicode-in-math-p t))


(defun user--tex-mode-config ()
  "Initialize generic text editing mode."
  (setq-default
   ;; (BibTeX) ;;
   bibtex-autokey-name-case-convert 'identity
   bibtex-autokey-year-length 4)

  (after-load 'mode-compile
    (setq mode-compile-modes-alist
          (append '((latex-mode . (tex-compile kill-compilation)))
                  mode-compile-modes-alist)))

  (after-load 'smartparens
    (sp-with-modes '(tex-mode plain-tex-mode latex-mode)
      (sp-local-tag "i" "\"<" "\">")
      (sp-local-pair "\\[" " \\]")
      (sp-local-pair "\\(" " \\)")
      (sp-local-pair "\\{" " \\}")
      (sp-local-pair "\\left(" " \\right)")
      (sp-local-pair "\\left\\{" " \\right\\}")))

  ;; Set up hooks.
  (add-hook 'tex-mode-hook 'user--tex-mode-hook)
  (add-hook 'TeX-mode-hook 'user--tex-mode-hook)
  (add-hook 'latex-mode-hook 'user--latex-mode-hook)
  (add-hook 'LaTeX-mode-hook 'user--latex-mode-hook)
  (add-hook 'bibtex-mode-hook 'user--bibtex-mode-hook)

  ;;; (Packages) ;;;
  (use-package auctex
    :defer t
    :bind (:map LaTeX-mode-map
           ([remap fill-paragraph] . LaTeX-fill-paragraph))
    :config (user--auctex-config))
  (use-package ebib
    :defer t
    :config (user--ebib-config))
  (use-package zotelo
    :defer t
    :config (user--zotelo-config))
  (use-package ac-math
    :defer t
    :config (user--ac-math-config))
  (require-package
   '(:name auto-complete-latex :after (user--auto-complete-latex-config)))
  (use-package company-auctex
    :defer t)
  (use-package company-math
    :defer t)
  (require-package '(:name ltx-help)))


(when (or (executable-find "tex")
          (executable-find "latex"))
  (user--tex-mode-config))


(provide 'modes/tex)
;;; tex.el ends here
