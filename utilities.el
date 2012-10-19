;; (Utilities) ;;

;; CEDET
(setq load-path (cons "~/.emacs.d/vendor/cedet/common" load-path))
(if (file-readable-p (expand-file-name "~/.emacs.d/vendor/cedet/common/cedet.elc"))
    (unless (featurep 'cedet)
      (require 'cedet)))
(when (featurep 'cedet)
  (global-ede-mode 1)
  (setq semantic-clang-binary "clang")
  (semantic-load-enable-minimum-features)
  (semantic-load-enable-code-helpers)
  (semantic-load-enable-gaudy-code-helpers)
  (semantic-load-enable-primary-exuberent-ctags-support)
  (global-semanticdb-minor-mode 1)
  (global-srecode-minor-mode 1)
  (setq semanticdb-default-save-directory "~/.emacs.cache/semanticdb"))


;; Emacs Code Browser
(setq load-path (cons "~/.emacs.d/vendor/ecb" load-path))
(require 'ecb)
(when (featurep 'ecb)
  (setq stack-trace-on-error nil)
  (custom-set-variables
    '(ecb-options-version "2.40")))


;; Visual popup user interface, required by auto-complete
(setq load-path (cons "~/.emacs.d/vendor/popup" load-path))


;; auto-complete-mode offers superior code completion over existing tools
(setq load-path (cons "~/.emacs.d/vendor/auto-complete" load-path))
(require 'auto-complete-config)
(when (featurep 'auto-complete)
  (ac-config-default)
  (setq ac-auto-start nil)
  (setq ac-quick-help-delay 0.5)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/vendor/auto-complete/ac-dict")
  ;; Store the completion history in the cache directory
  (setq ac-comphist-file "~/.emacs.cache/ac-comphist.dat")

  (when (featurep 'cedet)
    ;; Use semantic as a source for auto complete
    (setq ac-sources '(ac-source-semantic)))

  (setq load-path (cons "~/.emacs.d/vendor/auto-complete-clang" load-path))
  (require 'auto-complete-clang)
  (when (featurep 'auto-complete-clang)
    (add-hook 'c-mode-common-hook
              (lambda ()
                (setq ac-sources (append '(ac-source-clang) ac-sources)))))

  (add-hook 'auto-complete-mode-hook 'ac-common-setup)

  ;; Enable auto-complete globally
  (global-auto-complete-mode t))


;; XCscope
(setq load-path (cons "~/.emacs.d/vendor/xcscope" load-path))
(require 'xcscope)


;; Enable automatic detection of indentation style
(setq load-path (cons "~/.emacs.d/vendor/dtrt-indent" load-path))
(require 'dtrt-indent)


;; Close all open buffers
(defun close-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))


;; Show row and column numbers
(setq line-number-mode t)
(setq column-number-mode t)


;; Browsable kill ring
(setq load-path (cons "~/.emacs.d/vendor/browse-kill-ring" load-path))
(when (require 'browse-kill-ring nil 'noerror)
  (browse-kill-ring-default-keybindings))


;; Magit advanced Git integration
(setq load-path (cons "~/.emacs.d/vendor/magit" load-path))
(require 'magit)


;; SmartTab intelligent tab completion control
(setq load-path (cons "~/.emacs.d/vendor/smart-tab" load-path))
(require 'smart-tab)
(when (featurep 'smart-tab)
  (global-smart-tab-mode))


;; If running on windows load Outlook Edit
(setq load-path (cons "~/.emacs.d/vendor/outlookedit" load-path))
(if (eq system-type 'windows-nt)
    (require 'outlookedit))


;; LustyExplorer
(setq load-path (cons "~/.emacs.d/vendor/lusty-emacs" load-path))
(require 'lusty-explorer)
(when (featurep 'lusty-explorer)
  (global-set-key "\C-x\C-f" 'lusty-file-explorer)
  (global-set-key "\C-xb" 'lusty-buffer-explorer))


;; Wind Move enables window navigation
(require 'windmove)
(when (featurep 'windmove)
  (windmove-default-keybindings))


;; Visualize undo history as a tree structure
(setq load-path (cons "~/.emacs.d/vendor/undo-tree" load-path))
(require 'undo-tree)
(when (featurep 'undo-tree)
  (global-undo-tree-mode))


;; MultiTerm terminal
(setq load-path (cons "~/.emacs.d/vendor/multi-term" load-path))
(require 'multi-term)


;; Make buffer names unique
(require 'uniquify)
(when (featurep 'uniquify)
  (setq
   uniquify-buffer-name-style 'post-forward
   uniquify-separator ":"))


;; Enable TabBar minor mode
(setq load-path (cons "~/.emacs.d/vendor/tabbar" load-path))
(require 'tabbar)
(when (featurep 'tabbar-mode)
  (tabbar-mode))


;; Save/restore the position in Emacs buffers between sessions
(require 'saveplace)
(when (featurep 'saveplace)
  (setq save-place-file "~/.emacs.cache/saveplace")
  (setq-default save-place t))


;; Save/restore the history of various Emacs minibuffers
(require 'savehist)
(when (featurep 'savehist)
  (setq savehist-additional-variables '(search-ring regexp-search-ring kill-ring))
  (setq savehist-file "~/.emacs.cache/savehist")
  (savehist-mode t))


;; When using profile-dotemacs start with init.el
;; To profile run emacs (-Q) -l ~/.emacs.d/vendor/profile-dotemacs/profile-dotemacs.el -f profile-dotemacs
(setq profile-dotemacs-file "~/.emacs.d/init.el")


;; When using gud-mode to debug enable gdb-many-windows and a separate IO buffer
(setq gdb-many-windows t)
(setq gdb-use-separate-io-buffer t)


;; Fast and simple note taking
(setq load-path (cons "~/.emacs.d/vendor/deft" load-path))
(require 'deft)


;; Enable vc-clearcase so that VC speaks ClearCase
(setq load-path (cons "~/.emacs.d/vendor/vc-clearcase" load-path))
(require 'vc-clearcase)
(require 'ucm)


;; View Large File support
(setq load-path (cons "~/.emacs.d/vendor/vlf" load-path))
(require 'vlf)
(setq vlf-batch-size 52428800)


;; Sunrise Commander
(setq load-path (cons "~/.emacs.d/vendor/sunrise-commander" load-path))
(require 'sunrise-commander)
(add-to-list 'auto-mode-alist '("\\.srvm\\'" . sr-virtual-mode))


;; Multi Web Mode
(setq load-path (cons "~/.emacs.d/vendor/multi-web-mode" load-path))
(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                  (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                  (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
(multi-web-global-mode 1)


;; newLISP utility files
(setq load-path (cons "~/.emacs.d/vendor/newlisp" load-path))
(require 'newlisp)
(when (featurep 'newlisp)
  (add-to-list 'auto-mode-alist '("\\.lsp$" . newlisp-mode))
  (add-to-list 'interpreter-mode-alist '("newlisp" . newlisp-mode)))

(defun swank-newlisp-init (port-filename coding-system)
  (format "%S\n" `(swank:start-server ,port-filename)))

(defvar swank-newlisp-filename "swank-newlisp.lsp")
(defun slime-newlisp ()
  (interactive)
  (let ((slime-lisp-implementations
         `((newlisp ("newlisp" "-n" ,(locate-file swank-newlisp-filename load-path))
                    :init swank-newlisp-init
                    :coding-system utf-8-unix))))
    (slime 'newlisp)))


;; SLIME
(setq load-path (cons "~/.emacs.d/vendor/slime" load-path))
(setq inferior-lisp-program "newlisp")
(require 'slime)
(slime-setup)

