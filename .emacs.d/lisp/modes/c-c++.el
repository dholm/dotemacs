;;; c-c++.el --- initializes C/C++ modes
;;; Commentary:
;;; Code:

(defun user/c-mode-common-hook ()
  "C mode common hook."
  (setq
   ;; Indent using four spaces.
   c-basic-offset 4)

  ;; Override the indentation level of case labels in the K&R- and
  ;; Stroustrup styles so that they are indented one level beyond
  ;; the switch.
  (c-set-offset 'case-label '+)

  ;; Propertize "#if 0" regions as comments.
  (font-lock-add-keywords
   nil
   '((user/c-mode-font-lock-if0 (0 font-lock-comment-face prepend))) 'add-to-end)

  ;; Load CEDET
  (user/c-mode-cedet-hook)

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

  (with-feature 'cpputils-cmake
    (when (derived-mode-p 'c-mode 'c++-mode)
      ;; Enable CMake C/C++ utilities.
      (cppcm-reload-all)))

  (with-feature 'irony
    (when (member major-mode irony-supported-major-modes)
      ;; Better auto completion.
      (irony-mode t)))

  (with-feature 'rtags-ac
    (add-ac-sources 'ac-source-rtags))

  (with-feature 'auto-complete-c-headers
    (add-ac-sources 'ac-source-c-headers))

  (user/smartparens-enable)

  ;;; (Bindings) ;;;
  (when (feature-p 'iasm-mode)
    (user/bind-key-local :code :library-list 'iasm-disasm-link-buffer)
    (user/bind-key-local :code :disassemble 'iasm-goto-disasm-buffer))
  (with-feature 'clang-format
    (user/bind-key-local :code :tidy 'clang-format-region))
  (with-executable 'gdb
    (user/bind-key-local :debug :start 'realgud-gdb)))


(defun user/c-mode-cedet-hook ()
  "C mode CEDET hook."
  (with-feature 'semantic/bovine/c
    ;; Load extra semantic helpers.
    (require 'semantic/bovine/gcc)
    (require 'semantic/bovine/clang nil :noerror)

    (user/cedet-hook)

    ;;; (Bindings) ;;;
    (with-feature 'eassist
      (add-many-to-list
       'eassist-header-switches
       ;; Add "hh" as C++ header file.
       '("hh" . ("cpp" "cc"))
       '("cc" . ("h" "hh" "hpp")))

      (user/bind-key-local :nav :switch-spec-impl 'eassist-switch-h-cpp)))

  (with-feature 'function-args
    (function-args-mode t)
    (after-load 'diminish
      (diminish 'function-args-mode))))


(defun user/irony-mode-hook ()
  "Mode hook for irony."
  (with-feature 'irony-eldoc
    (irony-eldoc t))

  (with-feature 'flycheck-irony
    (flycheck-irony-setup))

  ;; Load flags from compilation database.
  (irony-cdb-autosetup-compile-options)

  ;;; (Bindings) ;;;
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async)
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


(defun user/irony-mode-init ()
  "Initialize irony mode."
  (setq-default
   ;; Install irony server in user's local path.
   irony-user-dir *user-local-directory*)

  (after-load 'irony-mode
    (when (feature-p 'auto-complete)
      (irony-enable 'ac))

    ;;; (Hooks) ;;;
    (add-hook 'irony-mode-hook 'user/irony-mode-hook)))


(defun user/cflow-init ()
  "Initialize cflow."
  (defun user/cflow-function (function-name)
    "Get call graph of inputed function. "
    (interactive (list (car (senator-jump-interactive "Function name: " nil nil nil))))
    (let* ((file-name (if (tramp-tramp-file-p buffer-file-name)
                          (tramp-file-name-localname
                           (tramp-dissect-file-name buffer-file-name))
                        buffer-file-name))
           (cmd (format "cflow -b -n -T --main=\"%s\" %s" function-name file-name))
           (cflow-buf-name (format "**cflow-%s:%s**"
                                   (file-name-nondirectory buffer-file-name)
                                   function-name))
           (cflow-buf (get-buffer-create cflow-buf-name)))
      (set-buffer cflow-buf)
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert (shell-command-to-string cmd))
      (pop-to-buffer cflow-buf)
      (goto-char (point-min))
      (cflow-mode))))


(defun user/cpputils-cmake-init ()
  "Initialize cpputils CMake."
  (setq-default
   ;; Disable Flymake.
   cppcm-write-flymake-makefile nil))


(defun user/c-c++-mode-init ()
  "Initialize C/C++ mode."
  (setq-default
   ;; Support completion using tab.
   c-tab-always-indent nil
   c-insert-tab-function 'indent-for-tab-command)

  (after-load 'cc-mode
    (add-many-to-list 'c-default-style
                      '(c-mode . "K&R")
                      '(c++-mode . "Stroustrup")))

  (after-load 'smartparens
    (sp-with-modes '(c-mode c++-mode)
      ;; Automatically add another newline before closing curly brace on enter.
      (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
      (sp-local-pair "/*" "*/" :post-handlers '((" | " "SPC")
                                                ("* ||\n[i]" "RET")))))

  (add-hook 'c-mode-common-hook 'user/c-mode-common-hook)

  ;; Detect if inside a C++ header file.
  (add-magic-mode 'c++-mode 'user/c++-header-file-p)

  ;;; (Packages) ;;;
  (require-package '(:name auto-complete-c-headers))
  (when (and (executable-find "cmake")
             (executable-find "clang")
             (executable-find "llvm-config"))
    (require-package '(:name irony-mode :after (user/irony-mode-init)))
    (require-package '(:name irony-eldoc))
    (require-package '(:name flycheck-irony)))
  (with-executable 'cmake
    (require-package '(:name cpputils-cmake :after (user/cpputils-cmake-init))))
  (with-executable 'clang
    (require-package '(:name clang-format)))
  (require-package '(:name function-args))
  (require-package '(:name google-c-style))
  (with-executable 'cflow
    (require-package '(:name cflow :after (user/cflow-init)))))

(user/c-c++-mode-init)


(provide 'modes/c-c++)
;;; c-c++.el ends here
