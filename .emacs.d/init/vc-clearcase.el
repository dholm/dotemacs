;; Enable vc-clearcase so that VC speaks ClearCase

(push "~/.emacs.d/vcs/vc-clearcase" load-path)
(require 'vc-clearcase)
(require 'ucm)
