(push "~/.emacs.d/utilities/browse-kill-ring" load-path)
(when (require 'browse-kill-ring nil 'noerror)
  (browse-kill-ring-default-keybindings))
