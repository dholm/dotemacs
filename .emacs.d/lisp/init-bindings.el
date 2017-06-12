;;; init-bindings.el --- sets up basic Emacs bindings -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defvar user/global-keymap nil
  "Global keymap.")

(defvar user/global-reverse-keymap nil
  "Global reverse keymap, mapping bindings back to functions.")

(defvar ctl-l-map (make-keymap)
  "Default keymap for \\<ctl-l-map> commands.")


;; Set up prefixes for command groups.
(defcustom user/view-prefix (kbd "C-x v")
  "Keyboard prefix to use for view commands."
  :type 'key-sequence
  :group 'user)

(defcustom user/help-prefix (kbd "C-c h")
  "Keyboard prefix to use for help commands."
  :type 'key-sequence
  :group 'user)

(defcustom user/documentation-prefix (kbd "C-c d")
  "Keyboard prefix to use for documentation commands."
  :type 'key-sequence
  :group 'user)

(defcustom user/code-prefix (kbd "C-c c")
  "Keyboard prefix to use for code manipulation commands."
  :type 'key-sequence
  :group 'user)

(defcustom user/code-eval-prefix (kbd "C-c c e")
  "Keyboard prefix to use for code evaluation commands."
  :type 'key-sequence
  :group 'user)

(defcustom user/vcs-prefix (kbd "C-c v")
  "Keyboard prefix to use for version control commands."
  :type 'key-sequence
  :group 'user)

(defcustom user/utilities-prefix (kbd "C-c u")
  "Keyboard prefix to use for utility commands."
  :type 'key-sequence
  :group 'user)

(defcustom user/apps-prefix (kbd "C-c a")
  "Keyboard prefix to use for application commands."
  :type 'key-sequence
  :group 'user)


(defconst user/prefix-list (list "C-x" "C-c" "C-l")
  "List of the registered prefix keys.")


(defun user/make-key (keys)
  "Convert KEYS into the internal Emacs key representation."
  (kbd (if (listp keys)
           (mapconcat 'identity (mapcar 'eval keys) " ")
         keys)))


(defun user/get-key (group operation)
  "Get the key from GROUP to bind for OPERATION."
  (let ((key (cdr (assq operation (cdr (assq group user/global-keymap))))))
    (if key
        (user/make-key key)
      (error (format "Group %s does not contain key for %s!"
                     (symbol-name group) (symbol-name operation))))))


(defun user/get-key-function (group operation)
  "Get the function bound to GROUP OPERATION."
  (car (cdr (assq operation
                  (cdr (assq group user/global-reverse-keymap))))))


(defun user/bind-key-global (group key function)
  "Bind GROUP KEY to FUNCTION globally."
  (let ((rev-group (assq group user/global-reverse-keymap)))
    (setq user/global-reverse-keymap
          (append `((,group . ,(append `((,key ,function)) (cdr rev-group))))
                  (delq (assoc group user/global-reverse-keymap)
                        user/global-reverse-keymap))))
  (global-set-key (user/get-key group key) function))


(defun user/bind-key-local (group key function)
  "Bind GROUP KEY to FUNCTION in the current keymap."
  (local-set-key (user/get-key group key) function))


(defun user/merge-keymap-groups (overlay base)
  "Merge OVERLAY keymap with BASE group."
  (let ((group-name (car base))
        (overlay-keys (cdr overlay))
        (base-keys (cdr base)))
    `((,group-name . ,(append overlay-keys base-keys)))))


(defun user/global-keymap-overlay (overlay)
  "Load keymap OVERLAY."
  (dolist (ovl-group overlay)
    (let ((ovl-gname (car ovl-group))
          (ovl-keys (cdr ovl-group)))
      (dolist (ovl-op (cdr ovl-group))
        (let ((ovl-oname (car ovl-op))
              (ovl-key (cdr ovl-op)))
          ;; TODO: Check that ovl-oname exists.
          (global-set-key (user/make-key ovl-key)
                          (user/get-key-function ovl-gname ovl-oname))))
      (let ((orig-group (assq ovl-gname user/global-keymap))
            (keymap-without-group (assq-delete-all ovl-gname user/global-keymap)))
        (setq user/global-keymap
              (append (user/merge-keymap-groups ovl-group orig-group)
                      keymap-without-group)))))
  t)


(defun user--global-keymap-config ()
  "Initialize the global keymap."
  (setq
   user/global-keymap
   '(;;; (Basic keys) ;;;
     (:basic . ((:open-file . "C-x C-f")
                (:open-file-context . "C-x f")
                (:view-file . "C-x C-v")
                (:open-buffer . "C-x b")
                (:open-buffer-context . "C-x M-b")

                (:save . "C-x C-s")
                (:save-as . "C-x M-s")
                (:close . "C-x k")
                (:quit . "C-x C-c")

                (:undo . "C-_")
                (:redo . "M-_")

                (:forward-line . "C-n")
                (:backward-line . "C-p")
                (:forward-word . "M-f")
                (:backward-word . "M-b")
                (:forward-expr . "C-M-f")
                (:backward-expr . "C-M-b")
                (:del-char-left . "C-h")
                (:del-char-right . "C-d")

                (:search-forward . "C-s")
                (:search-backward . "C-r")
                (:search-files . ("C-l M-f"))
                (:swoop . "C-l C-s")
                (:swoop-multi . "C-l C-M-s")

                (:selection-start . "C-SPC")
                (:selection-expand . "M-=")
                (:selection-next . "M-.")
                (:selection-prev . "M-,")
                (:selection-all . "C-c M-.")
                (:selection-edit-lines . "C-c M-e")

                (:widen . (user/view-prefix "n w"))
                (:narrow-to-page . (user/view-prefix "n p"))
                (:narrow-to-region . (user/view-prefix "n r"))
                (:narrow-to-function . (user/view-prefix "n f"))

                (:copy . "C-x C-w")
                (:cut . "C-x C-k")
                (:copy-expr . "C-M-w")
                (:cut-expr . "C-M-k")
                (:paste . "C-y")
                (:cycle-paste . "M-y")
                (:cut-word-left . "C-w")
                (:cut-word-right . "M-w")

                (:zoom . (user/view-prefix "z"))))

     ;;; (Emacs) ;;;
     (:emacs . ((:describe-bindings . (user/help-prefix "b"))
                (:describe-coding . (user/help-prefix "C"))
                (:describe-char . (user/help-prefix "c"))
                (:describe-face . (user/help-prefix "F"))
                (:describe-all-faces . (user/help-prefix "M-f"))
                (:describe-function . (user/help-prefix "f"))
                (:describe-key . (user/help-prefix "k"))
                (:describe-key-extensive . (user/help-prefix "K"))
                (:describe-variable . (user/help-prefix "v"))
                (:search-variable-value . (user/help-prefix "V"))
                (:describe-language . (user/help-prefix "L"))
                (:describe-mode . (user/help-prefix "m"))
                (:describe-symbol . (user/help-prefix "s"))
                (:describe-syntax . (user/help-prefix "S"))

                (:find-library . (user/help-prefix "l"))
                (:find-package . (user/help-prefix "p"))
                (:manual . (user/help-prefix "M"))
                (:elisp-search . (user/help-prefix "e"))
                (:tutorial . (user/help-prefix "t"))
                (:where-is . (user/help-prefix "w"))

                (:redraw . "C-l C-l")
                (:recenter . "C-l l")
                (:fullscreen . "C-c <C-return>")
                (:text-scale-increase . "C-+")
                (:text-scale-decrease . "C--")

                (:grow-vertical . "C-c C-p")
                (:shrink-vertical . "C-c C-n")
                (:grow-horizontal . "C-c C-f")
                (:shrink-horizontal . "C-c C-b")
                (:flop-frame . "C-c C-t")
                (:flip-frame . "C-c M-t")
                (:rotate-frame-forward . "C-c C-r")
                (:rotate-frame-backward . "C-c M-r")

                (:profiler-start . (user/utilities-prefix "p p"))
                (:profiler-stop . (user/utilities-prefix "p P"))
                (:profiler-report . (user/utilities-prefix "p r"))))

     ;;; (Documentation) ;;;
     (:doc . ((:apropos . (user/documentation-prefix "SPC"))
              (:manual . (user/documentation-prefix "m"))

              (:describe . (user/documentation-prefix "d"))
              (:describe-function . (user/documentation-prefix "f"))
              (:describe-variable . (user/documentation-prefix "v"))

              (:reference . (user/documentation-prefix "r"))))

     ;;; (Navigation) ;;;
     (:nav . ((:context . ("C-l SPC"))

              (:goto-line . ("C-l g"))
              (:go-forward . ("C-l f"))
              (:go-back . ("C-l b"))

              (:scroll-up . "M-n")
              (:scroll-down . "M-p")

              (:context-cycle . ("C-l C-c"))
              (:context-forward . ("C-l C-f"))
              (:context-backward . ("C-l C-b"))
              (:context-up . ("C-l C-p"))
              (:context-down . ("C-l C-n"))

              (:next . ("C-l n"))

              (:follow-symbol . ("C-l j"))
              (:find-symbol . ("C-l s"))
              (:jump-spec-impl . ("C-l i"))
              (:references . ("C-l r"))
              (:find-references . ("C-l M-r"))
              (:find-virtuals . ("C-l v"))
              (:switch-spec-impl . ("C-l h"))
              (:functions/toc . ("C-l t"))
              (:file-dependencies . ("C-l d"))
              (:history . ("C-l h"))

              (:open . (user/utilities-prefix "o"))))

     ;;; (Programming) ;;;
     (:code . ((:compile . (user/code-prefix "c"))
               (:run . (user/code-prefix "r"))
               (:test . (user/code-prefix "t"))
               (:compilation-result . (user/view-prefix "c"))

               (:bookmark-prefix . "C-c b")
               (:bookmark-toggle . "C-c b v")
               (:bookmark-next . "C-c b n")
               (:bookmark-prev . "C-c b p")

               (:comment . "M-;")
               (:document . (user/code-prefix "="))
               (:join-line . ((if (display-graphic-p) "C-x C-6" "C-x C-^")))
               (:align . ((if (display-graphic-p) "C-x C-5" "C-x C-]")))
               (:fill-paragraph . ((if (display-graphic-p) "C-x C-4" "C-x C-\\")))
               (:tidy . ("C-x ="))
               (:whitespace-auto-cleanup . (user/code-prefix "w"))

               (:itemize . (user/code-prefix "b"))
               (:enumerate . (user/code-prefix "e"))

               (:complete . "TAB")
               (:try-complete . "TAB")
               (:auto-complete . "C-TAB")

               (:unwrap-expr . "C-M-d")

               (:context-promote . (user/code-prefix "P"))
               (:context-demote . (user/code-prefix "N"))

               (:library-list . (user/code-prefix "l"))
               (:disassemble . (user/code-prefix "D"))

               (:warnings/errors . (user/code-prefix "E"))
               (:spellcheck-word . (user/code-prefix "s"))
               (:spellcheck-add-word . (user/code-prefix "S"))
               (:thesaurus-lookup . (user/code-prefix "t"))

               (:update-index . (user/code-prefix "i"))

               (:eval-expression . ("C-x C-e"))
               (:eval-buffer . (user/code-eval-prefix "b"))
               (:eval-function . (user/code-eval-prefix "f"))
               (:eval-selection . (user/code-eval-prefix "s"))

               (:insert-dependency . (user/code-eval-prefix "M-d"))

               (:macro-expand . (user/code-eval-prefix "m"))

               (:virtual . (user/code-prefix "v"))))

     ;;; (Debugging) ;;;
     (:debug . ((:start . (user/code-prefix "d"))

                (:break . (user/code-prefix "b"))
                (:trace . (user/code-prefix "T"))
                (:break-temporary . (user/code-prefix "t"))
                (:watch . (user/code-prefix "w"))

                (:run . (user/code-prefix "r"))
                (:continue . (user/code-prefix "c"))
                (:continue-stack . (user/code-prefix "f"))
                (:continue-until . (user/code-prefix "u"))
                (:step . (user/code-prefix "s"))
                (:step-instruction . (user/code-prefix "i"))
                (:next . (user/code-prefix "n"))

                (:stack-up . (user/code-prefix "p"))
                (:stack-down . (user/code-prefix "n"))
                (:show-value . (user/code-prefix "p"))))

     ;;; (Version Control) ;;;
     (:vcs . ((:status . (user/vcs-prefix "s"))
              (:history . (user/vcs-prefix "h"))
              (:version . (user/vcs-prefix "v"))
              (:describe . (user/vcs-prefix "d"))
              (:gutter . (user/vcs-prefix "g"))
              (:review . (user/vcs-prefix "r"))

              (:add-buffer . (user/vcs-prefix "a"))
              (:next-action . (user/vcs-prefix "SPC"))
              (:mergetool . (user/vcs-prefix "m"))

              (:search . (user/vcs-prefix "M-s"))
              (:time-machine . (user/vcs-prefix "t"))))

     ;;; (Utilities) ;;;
     (:util . ((:annotate-buffer . (user/utilities-prefix "a"))
               (:draw . (user/utilities-prefix "d"))

               (:ace-jump-mode . ("C-l a"))

               (:ecb-toggle . (user/utilities-prefix "e"))

               (:google . (user/utilities-prefix "g"))
               (:google-at-point . (user/documentation-prefix "g RET"))
               (:google-selection . (user/documentation-prefix "g SPC"))
               (:stack-overflow-search . (user/documentation-prefix "s"))

               (:notifications . (user/utilities-prefix "n"))

               (:perspective . ("C-x x s"))

               (:popwin-close . (user/view-prefix "0"))
               (:popwin-buffer . (user/view-prefix "p"))
               (:popwin-messages . (user/view-prefix "m"))

               (:undo-tree . (user/utilities-prefix "u"))

               (:wc-mode . (user/utilities-prefix "w"))))

     ;;; (Applications) ;;;
     (:apps . ((:packages . (user/apps-prefix "M-p"))

               (:shell . (user/apps-prefix "s"))
               (:processes . (user/apps-prefix "p"))
               (:services . (user/apps-prefix "P"))

               (:agenda . (user/apps-prefix "a"))
               (:notes . (user/apps-prefix "n"))
               (:todo . (user/apps-prefix "t"))
               (:capture-task . (user/apps-prefix "M-t"))
               (:information-db . (user/apps-prefix "D"))

               (:browse . (user/apps-prefix "b"))
               (:browse-external . (user/apps-prefix "B"))
               (:feed-reader . (user/apps-prefix "f"))
               (:stack-exchange . (user/apps-prefix "x"))
               (:weather . (user/apps-prefix "w"))
               (:cheat-sh . (user/apps-prefix "C"))

               (:email . (user/apps-prefix "e"))
               (:irc . (user/apps-prefix "i"))
               (:instant-messenger . (user/apps-prefix "I"))

               (:ipython-notebook . (user/apps-prefix "N"))

               (:music . (user/apps-prefix "m"))

               (:elnode . (user/apps-prefix "E"))

               (:calculator . (user/apps-prefix "c"))
               (:convert-unit . (user/apps-prefix "M-c"))
               (:statistics . (user/apps-prefix "R"))
               (:sage . (user/apps-prefix "S")))))))


(defun user--bindings-config ()
  "Initialize key bindings."
  (global-unset-key (kbd "C-l"))
  (define-prefix-command 'ctl-l-map)
  (global-set-key (kbd "C-l") 'ctl-l-map)

  ;; Initialize global keymap.
  (user--global-keymap-config)

  ;;; (Bindings) ;;;
  ;; Alias C-x C-m to M-x which is a bit awkward to reach.
  (global-set-key (kbd "C-x C-m") 'execute-extended-command)
  (global-set-key (kbd "C-x m") 'execute-extended-command))

(user--bindings-config)


(provide 'init-bindings)
;;; init-bindings.el ends here
