;;; tramp --- remote file access -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Override built-in TRAMP.
(quelpa '(tramp
          :fetcher git
          :url "git://git.savannah.gnu.org/tramp.git"
          :files ("lisp/*.el" "texi/*.texi")))

(use-package tramp
  :after dash
  :init
  (autoload 'tramp-check-proper-method-and-host "tramp.el")
  :config
  (validate-setq
   ;; Auto save storage.
   tramp-auto-save-directory (path-join *user-auto-save-directory* "tramp")
   ;; Default file transfer method.
   tramp-default-method "ssh"
   ;; Cache passwords.
   password-cache t
   password-cache-expiry 1000
   ;; SSH is properly configured to share connections.
   tramp-use-ssh-controlmaster-options nil
   ;; Skip looking for dir-local on remote system to speed up tramp.
   enable-remote-dir-locals nil
   ;; Preserve PATH on remote host.
   tramp-remote-path (delete 'tramp-default-remote-path tramp-remote-path))

  ;; Preserve PATH on remote host.
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)

  (tramp-set-completion-function
   "ssh" (mapcar
          (lambda (x) (list 'tramp-parse-sconfig x))
          (-remove
           (lambda (x) (not (file-exists-p x)))
           `(,(path-join "/" "etc" "ssh_config")
             ,(path-join "/" "etc" "ssh" "ssh_config")
             ,(path-join *user-home-directory* ".ssh" "config")))))

  (unless (fboundp 'tramp-compat-split-string)
    ;; Workaround for Python mode depending on the deleted TRAMP
    ;; function `tramp-compat-split-string'.
    (defun tramp-compat-split-string (string pattern)
      "Like `split-string' but omit empty strings.
In Emacs, (split-string \"/foo/bar\" \"/\") returns (\"foo\" \"bar\").
This is, the first, empty, element is omitted.  In XEmacs, the first
element is not omitted."
      (split-string string pattern 'omit)))

  (use-package tramp-cache
    :ensure tramp
    :config
    (validate-setq
     ;; Persistency files.
     tramp-persistency-file-name
     (path-join *user-cache-directory* "tramp")))

  (use-package helm-tramp
    :init
    (user/bind-key-global :basic :open-file-tramp 'helm-tramp)))


(provide 'utilities/tramp)
;;; tramp.el ends here
