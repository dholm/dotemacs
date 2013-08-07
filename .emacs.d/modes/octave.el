;;; octave.el --- Octave mode
;;; Commentary:
;;; Code:

(defconst *has-octave* (executable-find "octave"))


(defun user/octave-mode-hook ()
  "Initialize Octave mode."
  (when (el-get-package-is-installed 'ac-octave)
    (set (make-local-variable 'ac-sources)
         (append ac-sources '(ac-octave)))))

(defun user/octave-init ()
  "Initialize Emacs Octave support."
  (add-auto-mode 'octave-mode "\\.m$")
  (add-hook 'octave-mode-hook 'user/octave-mode-hook))

(when *has-octave*
  (user/octave-init)
  (require-package '(:name ac-octave)))


(provide 'modes/octave)
;;; octave.el ends here
