(defconst *has-clang* (executable-find "clang"))

;; Set the default C/C++ code style
(setq c-default-style "K&R")
(setq c++-default-style "Stroustrup")


(require-package (:name c-eldoc))
(when *has-clang*
  (require-package (:name clang-complete-async)))


(defun dholm/c-mode-common-hook ()
  ;; Load CEDET
  (dholm/c-mode-cedet-hook)
  ;; Enable eldoc
  (c-turn-on-eldoc-mode)
  ;; Override the indentation level of case labels in the K&R- and
  ;; Stroustrup styles so that they are indented one level beyond
  ;; the switch.
  (c-set-offset 'case-label '+)
  ;; Run spell-checker on strings and comments
  (flyspell-prog-mode)
  ;; Separate camel-case into separate words
  (subword-mode t)
  ;; Show trailing whitespace
  (setq show-trailing-whitespace t)
  (add-hook 'before-save-hook
            ;; Delete trailing whitespace on save
            'delete-trailing-whitespace nil t)
  ;; Autocompletion
  (when *has-clang*
    (set (make-local-variable 'ac-sources)
         (append ac-sources '(ac-source-clang-async)))
    (ac-clang-launch-completion-process))
  ;; Enable dtrt-indent to attempt to identify the indentation rules used
  (dtrt-indent-mode t))

(add-hook 'c-mode-common-hook 'dholm/c-mode-common-hook)


(provide 'modes/c-c++)
