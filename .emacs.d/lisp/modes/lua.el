;;; lua.el --- Lua mode support -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user--lua-mode-hook ()
  "Lua mode hook."
  (user/gnu-global-enable)

  (when (feature-p 'lua-block)
    (lua-block-mode t)))


(use-package lua-mode
  :if (executable-find "lua")
  :defer
  :init
  (add-hook 'lua-mode-hook 'user--lua-mode-hook)
  :config
  (use-package lua-block
    :requires lua-mode
    :quelpa (lua-block
             :fetcher url
             :url "http://www.emacswiki.org/emacs/download/lua-block.el")
    :init
    (autoload 'lua-block-mode "lua-block" nil t)))


(provide 'modes/lua)
;;; lua.el ends here
