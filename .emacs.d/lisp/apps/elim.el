;;; elim.el --- instant messenger -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(when (and (pkg-config-has-p "libxml-2.0")
           (pkg-config-has-p "purple"))
  (use-package elim
    :ensure nil
    :disabled
    :el-get t
    :bind-wrap
    ((:key :apps :instant-messenger) . garak)
    :config
    (with-eval-after-load 'lui
      (validate-setq
       lui-max-buffer-size 30000
       lui-flyspell-p t
       lui-flyspell-alist '(("." "american"))))
    (with-eval-after-load 'elim
      (validate-setq
       elim-directory (path-join *user-cache-directory* "elim")))))



(provide 'apps/elim)
;;; elim.el ends here
