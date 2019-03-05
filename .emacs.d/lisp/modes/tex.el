;;; tex.el --- TeX mode support -*- lexical-binding: t; -*-
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
        (with-eval-after-load 'sage
          ;; If Sage is available, it must be loaded first.
          (LaTeX-preview-setup))
      (LaTeX-preview-setup))
    (setq user/tex-preview-setup t))

  (turn-on-reftex)
  (visual-line-mode t)
  (user/smartparens-enable)

  (with-feature 'tex-fold
    (tex-fold-mode t))

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
  (when (feature-p 'latexdiff)
    (user/bind-key-local :util :diff 'helm-latexdiff))
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


(when (or (executable-find "tex")
          (executable-find "latex"))
  (use-package tex-mode
    :defer
    :hook
    ((tex-mode-hook . user--tex-mode-hook)
     (TeX-mode-hook . user--tex-mode-hook)
     (latex-mode-hook . user--latex-mode-hook)
     (LaTeX-mode-hook . user--latex-mode-hook)
     (bibtex-mode-hook . user--bibtex-mode-hook))
    :config
    (with-eval-after-load 'mode-compile
      (setq mode-compile-modes-alist
            (append '((latex-mode . (tex-compile kill-compilation)))
                    mode-compile-modes-alist)))

    ;;; (Packages) ;;;
    (use-package reftex
      :ensure nil
      :diminish reftex-mode
      :config
      (use-package company-reftex
        :after (company)))

    (use-package latex-extra
      :hook (LaTeX-mode-hook . latex-extra-mode))

    (use-package latexdiff
      :if (executable-find "latexdiff"))

    (use-package bibtex
      :config
      (validate-setq
       ;; (BibTeX) ;;
       bibtex-autokey-name-case-convert 'identity
       bibtex-autokey-year-length 4)

      (use-package gscholar-bibtex)
      (use-package bibtex-utils)
      (use-package bibclean-format
        :if (executable-find "bibclean")
        :hook (bibtex-mode-hook . bibclean-format-on-save-mode)))

    (use-package auctex
      :bind (:map LaTeX-mode-map
                  ([remap fill-paragraph] . LaTeX-fill-paragraph))
      :config
      (validate-setq
       ;; Use synctex to communicate with LaTeX.
       TeX-source-correlate-method 'synctex
       LaTeX-command "latex -synctex=1 -shell-escape"
       ;; Do not ask about saving buffers before starting TeX.
       TeX-save-query nil
       ;; Use PDF rather than DVI by default.
       TeX-PDF-mode t
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

      (with-eval-after-load 'preview
        ;; Support previewing of TikZ.
        (add-to-list 'preview-default-preamble "\\PreviewEnvironment{tikzpicture}" t))

      (with-eval-after-load 'auctex
        (when (user/company-mode-p)
          (with-feature 'company-auctex
            ;; Enable company AUCTeX completion.
            (company-auctex-init))))

      (cond
       ((eq system-type 'darwin)
        (validate-setq
         TeX-view-program-list (quote (("Preview" "open -a Preview.app %o")
                                       ("Skim" "open -a Skim.app %o")
                                       ("open" "open %o")))
         TeX-view-program-selection '((output-pdf "Preview"))))
       (t (with-executable 'evince
            (validate-setq
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
          (set-marker to-marker nil)))

      ;;; (Packages) ;;;
      (use-package auctex-latexmk
        :if (executable-find "latexmk")
        :init
        (auctex-latexmk-setup))

      (use-package auctex-lua
        :if (executable-find "luatex"))

      (use-package company-auctex
        :after (company)
        :init
        (company-auctex-init))

      (use-package bibretrieve))

    (use-package ebib
      :config
      ;; Cannot be validated as it breaks the specification of
      ;; `ebib-file-search-dirs'.
      (setq
       ebib-file-search-dirs
       '(lambda ()
          (with-project project (path-buffer-abs)
            (user/proj-root project)))))

    (use-package zotelo
      :diminish zotelo-minor-mode
      :hook (TeX-mode-hook . zotelo-minor-mode))

    (use-package ac-math
      :after (auto-complete)
      :config
      (validate-setq
       ;; Enable unicode math input.
       ac-math-unicode-in-math-p t))

    (use-package auto-complete-latex
      :after (auto-complete)
      :quelpa (auto-complete-latex
               :fetcher hg
               :url "https://bitbucket.org/tequilasunset/auto-complete-latex")
      :config
      (add-ac-modes 'latex-mode 'LaTeX-mode))

    (use-package company-math
      :after (company))

    (use-package ltx-help
      :quelpa (ltx-help
               :fetcher url
               :url "http://mirror.switch.ch/ftp/mirror/tex/info/latex2e-help-texinfo/ltx-help.el")))

  (use-package texfrag
    :diminish texfrag-mode
    :config
    (texfrag-global-mode t)))


(provide 'modes/tex)
;;; tex.el ends here
