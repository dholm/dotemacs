;;; buffers.el --- Configure Emacs buffers
;;; Commentary:
;;; Code:

(defun user/kill-matching-buffers (regexp &optional exclude-p)
  "Kill buffers whose name match the specified REGEXP but not EXCLUDE-P."
  (interactive "sKill buffers matching this regular expression: \nP")
  (dolist (buffer (buffer-list))
    (let ((name (buffer-name buffer)))
      (when (and name (not (string-equal name ""))
                 (/= (aref name 0) ?\s)
                 (string-match regexp name))
        (unless (and exclude-p (funcall exclude-p buffer))
          (kill-buffer buffer))))))


(defun user/kill-all-buffers ()
  "Close all open buffers."
  (interactive)
  (user/kill-matching-buffers "^[^\*].+[^\*]$")
  (switch-to-buffer "*scratch*"))


(defun user/kill-other-buffers ()
  "Close all open buffers except current one."
  (interactive)
  (user/kill-matching-buffers "^[^\*].+[^\*]$"
                              (lambda (x) (eq (current-buffer) x))))


(defun user/buffers-init ()
  "Initialize Emacs buffers."
  (setq-default
   ;; Set up uniquify's style.
   uniquify-buffer-name-style 'reverse
   uniquify-separator " • "
   uniquify-after-kill-buffer-p t
   uniquify-ignore-buffers-re "^\\*")

  ;;; (Bindings) ;;;
  (user/bind-key-global :basic :open-file 'find-file)
  (user/bind-key-global :basic :view-file 'view-file)
  (user/bind-key-global :basic :open-buffer 'switch-to-buffer)
  (user/bind-key-global :basic :save 'save-buffer)
  (user/bind-key-global :basic :save-as 'write-file)
  (user/bind-key-global :basic :close 'kill-buffer)
  (user/bind-key-global :basic :quit 'save-buffers-kill-terminal))

(user/buffers-init)


(provide 'ux/buffers)
;;; buffers.el ends here
