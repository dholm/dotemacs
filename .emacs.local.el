;;; .emacs.local.el --- local Emacs configuration
;;; Commentary:
;;; Code:

(after-load 'ede
  ;; Generic EDE project example.
  (when (file-exists-p "/path/to/project/root/file")
    (ede-cpp-root-project "Project"
                          :name "Project Name"
                          :file "/path/to/project/root/file"
                          :include-path '("libc" "libd")
                          :system-include-path '()
                          :spp-table '(("DEBUG" . ""))
                          :compile-command "nice make -j")))


;; Keymap override.
(user/global-keymap-overlay
 '((:basic . ((:open-file-context . "C-x f")))
   (:nav . ((:go-back . "C-x n")
            (:follow-symbol . "C-x m")))))


(provide '.emacs.local)
;;; .emacs.local.el ends here
