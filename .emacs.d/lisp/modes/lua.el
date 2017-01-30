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


(defun user--lua-mode-config ()
  "Initialize Lua mode."
  (use-package lua-mode
    :ensure t)
  (require-package '(:name lua-block
                           :type emacswiki
                           :website "https://raw.github.com/emacsmirror/emacswiki.org/master/lua-block.el"
                           :depends (lua-mode)
                           :prepare (progn
                                      (autoload 'lua-block-mode "lua-block" nil
                                        t))))

  (add-hook 'lua-mode-hook 'user--lua-mode-hook))

(with-executable 'lua
  (user--lua-mode-config))


(provide 'modes/lua)
;;; lua.el ends here
