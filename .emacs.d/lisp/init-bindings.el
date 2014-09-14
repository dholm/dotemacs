;;; init-bindings.el --- sets up basic Emacs bindings
;;; Commentary:
;;; Code:

(defvar user/global-keymap nil
  "Global keymap.")
(defvar user/global-reverse-keymap nil
  "Global reverse keymap, mapping bindings back to functions.")


;; Set up prefixes for command groups.
(defcustom user/navigation-prefix (kbd "C-c n")
  "Keyboard prefix to use for navigation commands."
  :type 'key-sequence
  :group 'user)

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


(defconst user/prefix-list (list "C-x" "C-c")
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


(defun user/global-keymap-init ()
  "Initialize the global keymap."
  (setq
   user/global-keymap
   '(;;; (Basic keys) ;;;
     (:basic . ((:open-file . "C-x C-f")
                (:open-file-context . "C-x f")
                (:view-file . "C-x C-v")
                (:open-buffer . "C-x b")

                (:save . "C-x C-s")
                (:save-as . "C-x M-s")
                (:close . "C-x k")
                (:quit . "C-x C-c")

                (:undo . "C-_")
                (:redo . "M-_")

                (:search-forward . "C-s")
                (:search-backward . "C-r")
                (:search-files . (user/navigation-prefix "f"))
                (:swoop . "M-i")
                (:swoop-multi . "C-M-i")

                (:selection-start . "C-SPC")
                (:selection-expand . "M-=")
                (:selection-next . "M-.")
                (:selection-prev . "M-,")
                (:selection-all . "C-c M-.")
                (:selection-edit-lines . "C-c M-e")

                (:copy . "C-x C-w")
                (:cut . "C-x C-k")
                (:paste . "C-y")
                (:cycle-paste . "M-y")
                (:cut-word-left . "C-w")
                (:cut-word-right . "M-w")

                (:zoom . (user/view-prefix "z"))))

     ;;; (Emacs) ;;;
     (:emacs . ((:describe-bindings . (user/help-prefix "b"))
                (:describe-coding . (user/help-prefix "C"))
                (:describe-char . (user/help-prefix "c"))
                (:describe-function . (user/help-prefix "f"))
                (:describe-key . (user/help-prefix "k"))
                (:describe-key-extensive . (user/help-prefix "K"))
                (:describe-variable . (user/help-prefix "v"))
                (:describe-language . (user/help-prefix "L"))
                (:describe-mode . (user/help-prefix "m"))
                (:describe-symbol . (user/help-prefix "s"))
                (:describe-syntax . (user/help-prefix "S"))

                (:find-library . (user/help-prefix "l"))
                (:find-package . (user/help-prefix "p"))
                (:manual . (user/help-prefix "M"))
                (:tutorial . (user/help-prefix "t"))
                (:where-is . (user/help-prefix "w"))

                (:fullscreen . "C-c <C-return>")

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
     (:nav . ((:context . (user/navigation-prefix "SPC"))

              (:goto-line . (user/navigation-prefix "g"))
              (:go-forward . (user/navigation-prefix "f"))
              (:go-back . (user/navigation-prefix "b"))

              (:context-cycle . (user/navigation-prefix "s c"))
              (:context-forward . (user/navigation-prefix "s f"))
              (:context-backward . (user/navigation-prefix "s b"))
              (:context-up . (user/navigation-prefix "s p"))
              (:context-down . (user/navigation-prefix "s n"))

              (:next . (user/navigation-prefix "n"))

              (:follow-symbol . (user/navigation-prefix "j"))
              (:find-symbol . (user/navigation-prefix "s"))
              (:jump-spec-impl . (user/navigation-prefix "i"))
              (:references . (user/navigation-prefix "r"))
              (:switch-spec-impl . (user/navigation-prefix "h"))
              (:functions/toc . (user/navigation-prefix "t"))
              (:history . (user/view-prefix "h"))

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

               (:comment . (user/code-prefix "-"))
               (:document . (user/code-prefix "="))
               (:align . (user/code-prefix "a"))
               (:fill-paragraph . (user/code-prefix "f"))

               (:itemize . (user/code-prefix "b"))
               (:enumerate . (user/code-prefix "e"))

               (:complete . "RETURN")
               (:try-complete . "TAB")
               (:auto-complete . "C-TAB")

               (:context-promote . (user/code-prefix "P"))
               (:context-demote . (user/code-prefix "N"))

               (:library-list . (user/code-prefix "l"))
               (:disassemble . (user/code-prefix "D"))

               (:warnings/errors . (user/code-prefix "E"))
               (:todos . (user/code-prefix "F"))
               (:spellcheck-word . (user/code-prefix "s"))
               (:spellcheck-add-word . (user/code-prefix "S"))

               (:update-index . (user/code-prefix "i"))

               (:eval-expression . ("C-x C-e"))
               (:eval-buffer . (user/code-prefix "e b"))
               (:eval-function . (user/code-prefix "e f"))
               (:eval-selection . (user/code-prefix "e s"))

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
              (:describe . (user/vcs-prefix "d"))
              (:gutter . (user/vcs-prefix "g"))
              (:review . (user/vcs-prefix "r"))

              (:add-buffer . (user/vcs-prefix "a"))
              (:next-action . (user/vcs-prefix "SPC"))
              (:mergetool . (user/vcs-prefix "m"))

              (:time-machine . (user/vcs-prefix "t"))))

     ;;; (Utilities) ;;;
     (:util . ((:annotate-buffer . (user/utilities-prefix "a"))

               (:ace-jump-mode . (user/navigation-prefix "a"))

               (:ecb-toggle . (user/utilities-prefix "e"))

               (:google . (user/utilities-prefix "g"))
               (:google-at-point . (user/documentation-prefix "g RET"))
               (:google-selection . (user/documentation-prefix "g SPC"))
               (:stack-overflow-search . (user/documentation-prefix "s"))

               (:macrostep-expand . (user/code-prefix "e m"))

               (:notifications . (user/utilities-prefix "n"))

               (:perspective . ("C-x x s"))

               (:popwin-close . (user/view-prefix "0"))
               (:popwin-buffer . (user/view-prefix "p"))
               (:popwin-messages . (user/view-prefix "m"))

               (:undo-tree . (user/utilities-prefix "u"))

               (:wc-mode . (user/utilities-prefix "w"))))

     ;;; (Applications) ;;;
     (:apps . ((:shell . (user/apps-prefix "s"))

               (:agenda . (user/apps-prefix "a"))
               (:notes . (user/apps-prefix "n"))
               (:capture-task . (user/apps-prefix "t"))

               (:browse . (user/apps-prefix "b"))
               (:browse-external . (user/apps-prefix "B"))
               (:feed-reader . (user/apps-prefix "f"))

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


(defun user/bindings-init ()
  "Initialize key bindings."
  ;; Initialize global keymap.
  (user/global-keymap-init)

  ;;; (Bindings) ;;;
  ;; Alias C-x C-m to M-x which is a bit awkward to reach.
  (global-set-key (kbd "C-x C-m") 'execute-extended-command)
  (global-set-key (kbd "C-x m") 'execute-extended-command))

(user/bindings-init)


(provide 'init-bindings)
;;; init-bindings.el ends here
