;;; lua.el --- Lua mode support
;;; Commentary:
;;; Code:

(defun user--lua-mode-hook ()
  "Lua mode hook."
  (user/gnu-global-enable)

  ;; Enable YouCompleteMe.
  (user/ycmd-enable)

  (when (feature-p 'lua-block)
    (lua-block-mode t)))


(with-executable 'lua
  (use-package lua-mode
    :defer
    :init
    (add-hook 'lua-mode-hook 'user--lua-mode-hook)
    :config
    (use-package lua-block
      :requires lua-mode
      :quelpa (lua-block
               :fetcher wiki
               :files ("lua-block.el"))
      :init
      (autoload 'lua-block-mode "lua-block" nil t))))


(provide 'modes/lua)
;;; lua.el ends here
