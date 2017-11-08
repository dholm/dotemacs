;;; c-c++.el --- initializes C/C++ modes -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user--c-mode-common-hook ()
  "C-like languages mode hook."
  (setq
   ;; Indent using four spaces.
   c-basic-offset 4)

  ;; Override the indentation level of case labels in the K&R- and
  ;; Stroustrup styles so that they are indented one level beyond
  ;; the switch.
  (c-set-offset 'case-label '+)
  ;; Don't indent when inside `extern "<lang>"'.
  (c-set-offset 'inextern-lang 0)

  ;; Enable Doxygen support.
  (doxygen-mode t)

  ;; Separate camel-case into separate words.
  (subword-mode t)

  (when (feature-p 'mic-paren)
    ;; Match context to open parentheses.
    (paren-toggle-open-paren-context t))

  (when (and *user-cedet-ectags-enabled*
             (feature-p 'helm-etags-plus))
    ;; Automatically update tags.
    (turn-on-ctags-auto-update-mode))

  (user/gnu-global-enable)
  (user/cscope-enable)

  (when (user/auto-complete-p)
    (when (and (require 'rtags-ac nil :noerror)
               (user/use-rtags))
      (add-ac-sources 'ac-source-rtags))
    (with-feature 'auto-complete-c-headers
      (add-ac-sources 'ac-source-c-headers)))

  (when (user/company-mode-p)
    (when (and (require 'company-rtags nil :noerror)
               (user/use-rtags))
      (validate-setq rtags-completions-enabled t)
      (add-company-sources 'company-rtags))
    (with-feature 'company-c-headers
      (add-company-sources 'company-c-headers)))

  (when (and (require 'flycheck-rtags nil :noerror)
             (user/use-rtags))
    (flycheck-select-checker 'rtags))

  (when (feature-p 'helm-ctest)
    (user/bind-key-local :code :test 'helm-ctest))

  (user/smartparens-enable))


(defun user--c-mode-hook ()
  "C mode hook."
  ;; Propertize "#if 0" regions as comments.
  (font-lock-add-keywords
   nil
   '((user/c-mode-font-lock-if0 (0 font-lock-comment-face prepend)))
   'add-to-end)

  ;; Load CEDET
  (user--c-mode-cedet-hook)

  (with-feature 'cpputils-cmake
    ;; Enable CMake C/C++ utilities.
    (cppcm-reload-all))

  (with-feature 'irony
    (when (member major-mode irony-supported-major-modes)
      ;; Better auto completion.
      (irony-mode t)))

  ;;; (Bindings) ;;;
  (when (feature-p 'iasm-mode)
    (user/bind-key-local :code :library-list 'iasm-disasm-link-buffer)
    (user/bind-key-local :code :disassemble 'iasm-goto-disasm-buffer))
  (with-feature 'clang-format
    (user/bind-key-local :code :tidy 'clang-format-region))
  (with-executable 'gdb
    (user/bind-key-local :debug :start 'realgud-gdb)))


(defun user--c-mode-cedet-hook ()
  "C mode CEDET hook."
  (with-feature 'semantic/bovine/c
    ;; Load extra semantic helpers.
    (require 'semantic/bovine/gcc)
    (require 'semantic/bovine/clang nil :noerror)

    (user--cedet-hook)

    ;;; (Bindings) ;;;
    (with-feature 'eassist
      (add-many-to-list
       'eassist-header-switches
       ;; Add "hh" as C++ header file.
       '("hh" . ("cpp" "cc"))
       '("cc" . ("h" "hh" "hpp")))

      (user/bind-key-local :nav :switch-spec-impl 'eassist-switch-h-cpp))))


(defun user--irony-mode-hook ()
  "Mode hook for irony."
  (with-feature 'irony-eldoc
    (irony-eldoc t))

  (with-feature 'flycheck-irony
    (flycheck-irony-setup))

  ;; Load flags from compilation database.
  (irony-cdb-autosetup-compile-options)

  ;;; (Bindings) ;;;
  (define-key irony-mode-map (user/get-key :code :auto-complete)
    'irony-completion-at-point-async))


(defun user/c-mode-font-lock-if0 (limit)
  "Propertize '#if 0' regions, up to LIMIT in size, as comments."
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-min))
      (let ((depth 0) str start start-depth)
        (while (re-search-forward "^\\s-*#\\s-*\\(if\\|else\\|endif\\)" limit 'move)
          (setq str (match-string 1))
          (if (string= str "if")
              (progn
                (setq depth (1+ depth))
                (when (and (null start) (looking-at "\\s-+0"))
                  (setq start (match-end 0)
                        start-depth depth)))
            (when (and start (= depth start-depth))
              (c-put-font-lock-face start (match-beginning 0) 'font-lock-comment-face)
              (setq start nil))
            (when (string= str "endif")
              (setq depth (1- depth)))))
        (when (and start (> depth 0))
          (c-put-font-lock-face start (point) 'font-lock-comment-face)))))
  nil)


(defun user/c++-header-file-p ()
  "Return non-nil if in a C++ header."
  (and (string-match "\\.h$"
                   (or (buffer-file-name)
                      (buffer-name)))
     (save-excursion
       (re-search-forward "\\_<class\\_>" nil t))))


(use-package cc-mode
  :defer
  :init
  (add-hook 'c-mode-common-hook 'user--c-mode-common-hook)
  (add-hook 'c-mode-hook 'user--c-mode-hook)
  ;; Detect if inside a C++ header file.
  (add-magic-mode 'c++-mode 'user/c++-header-file-p)
  :config
  (add-many-to-list
   'c-default-style
   ;; Default mode for C.
   '(c-mode . "K&R")
   ;; Default mode for C++.
   '(c++-mode . "Stroustrup"))

  (with-eval-after-load 'smartparens
    (sp-with-modes '(c-mode c++-mode)
                   ;; Automatically add another newline before closing curly brace on enter.
                   (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
                   (sp-local-pair "/*" "*/" :post-handlers '((" | " "SPC")
                                                             ("* ||\n[i]" "RET")))))

  ;;; (Packages) ;;;
  (use-package cc-vars
    :ensure cc-mode
    :config
    (validate-setq
     ;; Support completion using tab.
     c-tab-always-indent nil
     c-insert-tab-function 'indent-for-tab-command))

  (use-package auto-complete-c-headers)
  (use-package company-c-headers)
  (use-package flycheck-pkg-config
    :if (executable-find "pkg-config"))
  (use-package cpputils-cmake
    :if (executable-find "cmake")
    :config
    (validate-setq
     ;; Disable Flymake.
     cppcm-write-flymake-makefile nil))
  (use-package cmake-ide
    :if (executable-find "cmake"))
  (use-package clang-format
    :if (executable-find "clang"))
  (use-package helm-ctest
    :if (executable-find "ctest"))
  (use-package irony
    :disabled
    :if (and (executable-find "clang") (executable-find "cmake"))
    :bind (:map irony-mode-map
                ([remap completion-at-point] . irony-completion-at-point-async)
                ([remap complete-symbol] . irony-completion-at-point-async))
    :diminish irony-mode
    :init
    (add-hook 'irony-mode-hook 'user--irony-mode-hook)
    :config
    (validate-setq
     ;; Install irony server in user's local path.
     irony-user-dir *user-local-directory*)

    (when (user/auto-complete-p)
      (irony-enable 'ac))

    (use-package irony-eldoc)
    (use-package flycheck-irony))
  (use-package flycheck-clang-analyzer
    :after flycheck
    :if (executable-find "scan-build")
    :config (flycheck-clang-analyzer-setup))
  (use-package function-args
    :pin "MELPA"
    :diminish function-args-mode
    :config
    (fa-config-default))
  (use-package google-c-style))


(provide 'modes/c-c++)
;;; c-c++.el ends here
