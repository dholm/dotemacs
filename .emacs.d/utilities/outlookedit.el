(push "~/.emacs.d/utilities/outlookedit" load-path)
(if (eq system-type 'windows-nt)
    (require 'outlookedit))
