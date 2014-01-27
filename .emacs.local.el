;;; emacs.local.el --- local Emacs configuration
;;; Commentary:
;;; Code:

(after-load 'ede
  ;; Generic EDE project example.
  (when (file-exists-p "/path/to/project/root/file")
    (ede-cpp-root-project "Project"
                          :file "/path/to/project/root/file"
                          :include-path '("libc" "libd")
                          :system-include-path '()
                          :spp-table '(("DEBUG" . ""))
                          :compile-command "nice make -j")))


(provide '.emacs.local)
;;; emacs.local.el ends here
