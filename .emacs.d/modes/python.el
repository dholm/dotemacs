;; (Code Conventions) ;;

;; Ropemacs Python refactoring library
(require 'pymacs)
(pymacs-load "ropemacs" "rope-")


;;; Enable installed helpers for Python
(add-hook 'python-mode-hook
          (lambda ()
            ;; Run spell-checker on strings and comments
            (flyspell-prog-mode)
            (flymake-mode)
            ;; Separate camel-case into separate words
            (subword-mode t)
            ;; Show trailing whitespace
            (setq show-trailing-whitespace t)
            (add-hook 'before-save-hook
                      ;; Delete trailing whitespace on save
                      'delete-trailing-whitespace nil t)))


;; (Utilities) ;;

;; Python mode for Emacs
(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
(require 'python)


;; Pylookup
(setq pylookup-dir "~/.emacs.d/el-get/pylookup")
(push pylookup-dir load-path)
(require 'pylookup)
;; set executable file and db file
(setq pylookup-program (concat pylookup-dir "/pylookup.py"))
(setq pylookup-db-file (concat pylookup-dir "/pylookup.db"))
;; set search option if you want
;; (setq pylookup-search-options '("--insensitive" "0" "--desc" "0"))
;; to speedup, just load it on demand
(autoload 'pylookup-lookup "pylookup"
  "Lookup SEARCH-TERM in the Python HTML indexes." t)
(autoload 'pylookup-update "pylookup" 
  "Run pylookup-update and create the database at `pylookup-db-file'." t)


;; Multiple Python checkers support
(when (load "flymake" t)
  (defun flymake-pycheckers-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "/path/to/this/file" (list local-file)))))


; Run Python checkers when in flymake-mode
(when (load "flymake" t)
  (defun flymake-pycheckers-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "~/.emacs.d/bin/pycheckers"  (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pycheckers-init)))
