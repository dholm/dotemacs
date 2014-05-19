;;; c-c++.el --- initializes C/C++ modes
;;; Commentary:
;;; Code:

(defun user/c-mode-common-hook ()
  "C mode common hook."
  ;; Indent using four spaces.
  (setq c-basic-offset 4)

  ;; Override the indentation level of case labels in the K&R- and
  ;; Stroustrup styles so that they are indented one level beyond
  ;; the switch.
  (c-set-offset 'case-label '+)

  ;; Load CEDET
  (user/c-mode-cedet-hook)

  ;; Enable Doxygen support.
  (doxygen-mode t)

  ;; Separate camel-case into separate words.
  (subword-mode t)

  (when (feature-p 'mic-paren)
    ;; Match context to open parentheses.
    (paren-toggle-open-paren-context t))

  (when (feature-p 'helm-etags-plus)
    ;; Automatically update tags.
    (turn-on-ctags-auto-update-mode))

  (when (feature-p 'helm-gtags)
    ;; Enable helm-gtags which in turn enables auto-update of Global tags.
    (helm-gtags-mode t))

  ;; Auto completion.
  (with-executable 'clang
    (with-feature 'auto-complete-clang
      (add-ac-sources 'ac-source-clang)))

  ;;; (Bindings) ;;;
  (with-executable 'gdb
    (user/bind-key-local :debug :start 'realgud-gdb)))


(defun user/c-mode-cedet-hook ()
  "C mode CEDET hook."
  (with-feature 'semantic/bovine/c
    ;; Load extra semantic helpers.
    (require 'semantic/bovine/gcc)
    (require 'semantic/bovine/clang)

    (user/cedet-hook)

    ;;; (Bindings) ;;;
    (user/bind-key-local :nav :switch-spec-impl 'eassist-switch-h-cpp)))


(defun user/c++-header-file-p ()
  "Return non-nil if in a C++ header."
  (and (string-match "\\.h$"
                   (or (buffer-file-name)
                      (buffer-name)))
     (save-excursion
       (re-search-forward "\\_<class\\_>" nil t))))


(defun user/c-c++-mode-init ()
  "Initialize C/C++ mode."
  (when (and (executable-find "clang")
             (executable-find "llvm-config"))
    (require-package '(:name auto-complete-clang)))

  (after-load 'cc-mode
    (add-many-to-list 'c-default-style
                      '(c-mode . "K&R")
                      '(c++-mode . "Stroustrup")))

  (add-hook 'c-mode-common-hook 'user/c-mode-common-hook)

  ;; Detect if inside a C++ header file.
  (add-magic-mode 'c++-mode 'user/c++-header-file-p)

  ;;; (Packages) ;;;
  (require-package '(:name google-c-style)))

(user/c-c++-mode-init)


(provide 'modes/c-c++)
;;; c-c++.el ends here
