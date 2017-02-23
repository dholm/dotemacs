;;; calc.el --- Emacs calculator
;;; Commentary:
;;; Code:

(use-package calc
  :commands calc
  :init
  (user/bind-key-global :apps :calculator 'calc)
  :config
  (validate-setq
   ;; Location of user calculator configuration.
   calc-settings-file (path-join *user-data-directory* "calc.el")
   ;; Increase calc's undo history.
   calc-undo-length 1000
   ;; Use a different face to display sub-formulas.
   calc-highlight-selections-with-faces t)

  (use-package calc-units
    :config
    ;; Add additional units for bits and bytes.
    ;; Stolen from: `https://github.com/dalehagglund/emacs.d/blob/master/calc.el'
    (add-many-to-list
     'math-additional-units
     '(bit nil "basic unit of information")
     '(byte "8 * bit" "eight bits")
     '(B "byte" "one byte")
     '(KiB "1024 * B" "kibibyte")
     '(MiB "1024 * KiB" "mebibyte")
     '(GiB "1024 * MiB" "gibibyte")
     '(TiB "1024 * GiB" "tebibyte")
     '(PiB "1024 * TiB" "pebibyte")
     '(EiB "1024 * PiB" "exbibyte")
     '(ZiB "1024 * EiB" "zebibyte")
     '(YiB "1024 * ZiB" "yobibyte")))

  ;; Allow yanking using the mouse.
  (define-key calc-mode-map [mouse-2] 'calc-yank)

  (use-package easy-convert
    :quelpa (easy-convert
             :fetcher github
             :repo "Frozenlock/easy-convert")
    :commands easy-convert-interactive
    :init
    (autoload 'easy-convert-interactive "easy-convert" nil t)
    (user/bind-key-global :apps :convert-unit 'easy-convert-interactive)))


(provide 'apps/calc)
;;; calc.el ends here
