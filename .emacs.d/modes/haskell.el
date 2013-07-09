;; Haskell mode

(defun dholm/haskell-mode-hook ()
  (turn-on-haskell-doc-mode)
  (turn-on-haskell-indentation)
  ;; Run spell-checker on strings and comments
  (flyspell-prog-mode))
(add-hook 'haskell-mode-hook 'dholm/haskell-mode-hook)
