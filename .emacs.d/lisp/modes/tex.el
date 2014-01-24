;;; tex.el --- TeX mode support
;;; Commentary:
;;; Code:

(defun user/tex-mode-hook ()
  "TeX mode hook."
  (turn-on-reftex)
  (outline-minor-mode t)
  (visual-line-mode t)

  (when (el-get-package-is-installed 'mode-compile)
    ;; Override AUCTeX in favor of mode-compile.
    (kill-local-variable 'compile-command))

  ;; Enable AUCTeX auto completion when available.
  (when (el-get-package-is-installed 'auto-complete-auctex)
    (ac-auctex-setup))

  ;;; (Bindings) ;;;
  (define-key user/navigation-map (kbd "c") 'reftex-toc)
  (when (display-graphic-p)
    (define-key user/code-map (kbd "p") 'preview-document)))


(defun user/latex-mode-hook ()
  "LaTeX mode hook."
  ;; Enable TeX math macros.
  (LaTeX-math-mode t)

  ;; Enable auto-completion systems.
  (when (el-get-package-is-installed 'ac-math)
    (add-ac-sources 'ac-source-math-unicode 'ac-source-math-latex
                    'ac-source-latex-commands))

  ;; Activate improved sentence filling.
  (ad-activate 'LaTeX-fill-region-as-paragraph)

  ;;; (Bindings) ;;;
  (define-key user/code-map (kbd "f") 'LaTeX-fill-paragraph))


(defun user/bibtex-mode-hook ()
  "BibTeX mode hook.")


(defun user/auctex-init ()
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

  (when (display-graphic-p)
    ;; Setup LaTeX preview.
    (LaTeX-preview-setup))

  (cond
   ((eq system-type 'darwin)
    (setq-default
     TeX-view-program-list (quote (("Preview" "open -a Preview.app %o")
                                   ("Skim" "open -a Skim.app %o")
                                   ("open" "open %o")))
     TeX-view-program-selection '((output-pdf "Preview"))))
   (t (when *has-evince*
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
      (set-marker to-marker nil)))

  ;;; (Faces) ;;;
  (after-load 'solarized-theme
    (solarized-with-values
      (eval
       `(custom-theme-set-faces
         'solarized
         '(font-latex-bold-face ((t (:inherit bold :foreground ,solarized-emph))))
         '(font-latex-doctex-documentation-face ((t (:background unspecified))))
         '(font-latex-doctex-preprocessor-face
           ((t (:inherit (font-latex-doctex-documentation-face font-lock-builtin-face font-lock-preprocessor-face)))))
         '(font-latex-italic-face ((t (:inherit italic :foreground ,solarized-emph))))
         '(font-latex-math-face ((t (:foreground ,violet))))
         '(font-latex-sectioning-0-face ((t (:inherit font-latex-sectioning-1-face :height ,solarized-height-plus-1))))
         '(font-latex-sectioning-1-face ((t (:inherit font-latex-sectioning-2-face :height ,solarized-height-plus-1))))
         '(font-latex-sectioning-2-face ((t (:inherit font-latex-sectioning-3-face :height ,solarized-height-plus-1))))
         '(font-latex-sectioning-3-face ((t (:inherit font-latex-sectioning-4-face :height ,solarized-height-plus-1))))
         '(font-latex-sectioning-4-face ((t (:inherit font-latex-sectioning-5-face :height ,solarized-height-plus-1))))
         '(font-latex-sectioning-5-face ((t (:inherit ,s-variable-pitch :foreground ,yellow :weight bold))))
         '(font-latex-sedate-face ((t (:foreground ,solarized-emph))))
         '(font-latex-slide-title-face ((t (:inherit (,s-variable-pitch font-lock-type-face) :weight bold :height ,solarized-height-plus-3))))
         '(font-latex-string-face ((t (:foreground ,cyan))))
         '(font-latex-subscript-face ((t (:height ,solarized-height-minus-1))))
         '(font-latex-superscript-face ((t (:height ,solarized-height-minus-1))))
         '(font-latex-verbatim-face ((t (:inherit fixed-pitch :foreground ,solarized-fg :slant italic))))
         '(font-latex-warning-face ((t (:inherit bold :foreground ,orange)))))))))


(defun user/ebib-init ()
  "Initialize Ebib."
  (setq-default
   ebib-file-search-dirs `(user/current-path-apply 'user/project-root))

  ;;; (Bindings) ;;;
  (define-key user/utilities-map (kbd "r") 'ebib))


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

  (after-load 'mode-compile
    (setq mode-compile-modes-alist
          (append '((latex-mode . (tex-compile kill-compilation)))
                  mode-compile-modes-alist)))

  ;; Set up hooks.
  (add-hook 'tex-mode-hook 'user/tex-mode-hook)
  (add-hook 'TeX-mode-hook 'user/tex-mode-hook)
  (add-hook 'latex-mode-hook 'user/latex-mode-hook)
  (add-hook 'LaTeX-mode-hook 'user/latex-mode-hook)
  (add-hook 'bibtex-mode-hook 'user/bibtex-mode-hook)

  ;;; (Packages) ;;;
  (require-package '(:name auctex :after (user/auctex-init)))
  (require-package '(:name ebib :after (user/ebib-init)))
  (require-package '(:name zotelo :after (user/zotelo-init)))
  (require-package '(:name ac-math :after (user/ac-math-init)))
  (require-package '(:name auto-complete-latex :after (user/auto-complete-latex)))
  (require-package '(:name auto-complete-auctex
                           :type github
                           :pkgname "monsanto/auto-complete-auctex"
                           :prepare (autoload 'ac-auctex-setup "auto-complete-auctex"))))


(when (or *has-tex* *has-latex*)
  (user/tex-mode-init))


(provide 'modes/tex)
;;; tex.el ends here
