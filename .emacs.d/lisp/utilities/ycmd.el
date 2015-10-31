;;; ycmd.el --- YouCompleteMe server support
;;; Commentary:
;;; Code:

(defconst *user-ycmd-path*
  (path-join *user-home-directory* ".local" "opt" "ycmd"))
(defconst *user-ycmd-config*
  (path-join *user-home-directory* ".local" "etc" "ycmd.json"))


(defun user/ycmd-enable ()
  "Enable ycmd in current buffer."
  (interactive)
  (with-feature 'ycmd
    (ycmd-mode t)
    (with-feature 'auto-complete-ycmd
      (add-ac-sources 'ac-source-ycmd))))


(defun user/ycmd-init ()
  "Initialize ycmd."
  (setq-default
   ycmd-server-command `("python" ,(path-join *user-ycmd-path* "ycmd"))
   ycmd-global-config *user-ycmd-config*
   ycmd-extra-conf-whitelist (path-join *user-ycmd-path* "cpp/ycm/.ycm_extra_conf.py"))

  (after-load 'flycheck
    (flycheck-ycmd-setup)))

(when (file-exists-p *user-ycmd-path*)
  (require-package '(:name emacs-ycmd :after (user/ycmd-init))))


(provide 'utilities/ycmd)
;;; ycmd.el ends here
