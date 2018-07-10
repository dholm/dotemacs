;;; coin-ticker.el --- Emacs cryptocurrency ticker -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package coin-ticker
  :disabled
  :config
  (validate-setq
   ;; Update interval.
   coin-ticker-api-poll-interval 120
   ;; Default to euro.
   coin-ticker-price-convert "EUR"
   coin-ticker-price-symbol "€")

  ;; Enable coin ticker mode.
  (coin-ticker-mode t))


(provide 'apps/coin-ticker)
;;; coin-ticker.el ends here
