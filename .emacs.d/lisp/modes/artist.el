;;; artist.el --- Initializes artist mode
;;; Commentary:
;;; Code:

(defun user/artist-mode-hook ()
  "Artist mode hook."
  ;;; (Bindings) ;;;
  (local-set-key (kbd "C-c C-a C-s") 'user/artist-select-setting))


(defun user/artist-select-setting (setting)
  "Interactively select a SETTING to change in `artist-mode'."
  (interactive
   (list
    (completing-read "Setting: "
                     (list "Set Fill" "Set Line" "Set Erase" "Spray-size" "Spray-chars"
                           "Rubber-banding" "Trimming" "Borders"))))
  (if (equal setting "Spray-size")
      (artist-select-operation "spray set size")
    (call-interactively
     (artist-fc-get-fn-from-symbol
      (cdr (assoc setting '(("Set Fill" . set-fill)
                            ("Set Line" . set-line)
                            ("Set Erase" . set-erase)
                            ("Rubber-banding" . rubber-band)
                            ("Trimming" . trimming)
                            ("Borders" . borders)
                            ("Spray-chars" . spray-chars))))))))


(defun user/artist-mode-init ()
  "Initialize artist mode."
  ;;; (Hooks) ;;;
  (add-hook 'picture-mode-hook 'user/artist-mode-hook)

  ;;; (Bindings) ;;;
  (user/bind-key-global :util :draw 'artist-mode))

(user/artist-mode-init)


(provide 'modes/artist)
;;; artist.el ends here
