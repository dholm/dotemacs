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
    (cond
     ((user/auto-complete-p)
      (with-feature 'auto-complete-ycmd
        (add-ac-sources 'ac-source-ycmd)))
     ((user/company-mode-p)
      (with-feature 'company-ycmd
        (add-company-sources 'company-ycmd))))

    (when (and (boundp 'flycheck-mode) flycheck-mode)
      ;; Integrate ycmd with flycheck.
      (with-feature 'flycheck-ycmd
        (add-hook 'ycmd-file-parse-result-hook 'flycheck-ycmd--cache-parse-results)
        (add-to-list 'flycheck-checkers 'ycmd)))))

(when (file-exists-p *user-ycmd-path*)
  (use-package ycmd
    :defer
    :config
    (validate-setq
     ycmd-server-command `("python" ,(path-join *user-ycmd-path* "ycmd"))
     ycmd-global-config *user-ycmd-config*
     ycmd-extra-conf-whitelist (path-join *user-ycmd-path* "cpp/ycm/.ycm_extra_conf.py"))

    (after-load 'flycheck
      (flycheck-ycmd-setup))))


(provide 'utilities/ycmd)
;;; ycmd.el ends here
