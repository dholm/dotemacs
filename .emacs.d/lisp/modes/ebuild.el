;;; ebuild.el --- Gentoo ebuild mode support. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package ebuild-mode
  :disabled
  :defer
  :quelpa (ebuild-mode
           :fetcher git
           :url "https://anongit.gentoo.org/git/proj/emacs-tools.git"
           :branch "ebuild-mode"))


(provide 'modes/ebuild)
;;; ebuild.el ends here
