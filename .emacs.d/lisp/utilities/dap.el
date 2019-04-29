;;; dap.el --- Debug adapter protocol. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package dap-mode
  :hook ((lsp-mode-hook . dap-mode)
         (lsp-mode-hook . dap-ui-mode))
  :bind-wrap
  (:map lsp-mode-map
        ((:key :debug :break) . dap-breakpoint-toggle)
        ((:key :debug :step) . dap-step-in)
        ((:key :debug :next) . dap-next)
        ((:key :debug :run) . dap-debug)
        ((:key :debug :continue) . dap-continue)

        ((:key :debug :show-value) . dap-ui-inspect-thing-at-point)))


(provide 'utilities/dap)
;;; dap.el ends here
