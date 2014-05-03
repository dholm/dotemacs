;;; completion.el --- Configures automatic code completion
;;; Commentary:
;;; Code:

(defun ac-pcomplete ()
  "Auto-complete source for pcomplete."
  ;; eshell uses `insert-and-inherit' to insert a \t if no completion
  ;; can be found, but this must not happen as auto-complete source
  (flet ((insert-and-inherit (&rest args)))
    ;; this code is stolen from `pcomplete' in pcomplete.el
    (let* (tramp-mode ;; do not automatically complete remote stuff
           (pcomplete-stub)
           (pcomplete-show-list t) ;; inhibit patterns like * being deleted
           pcomplete-seen pcomplete-norm-func
           pcomplete-args pcomplete-last pcomplete-index
           (pcomplete-autolist pcomplete-autolist)
           (pcomplete-suffix-list pcomplete-suffix-list)
           (candidates (pcomplete-completions))
           (beg (pcomplete-begin))
           ;; note, buffer text and completion argument may be
           ;; different because the buffer text may bet transformed
           ;; before being completed (e.g. variables like $HOME may be
           ;; expanded)
           (buftext (buffer-substring beg (point)))
           (arg (nth pcomplete-index pcomplete-args)))
      ;; we auto-complete only if the stub is non-empty and matches
      ;; the end of the buffer text
      (when (and (not (zerop (length pcomplete-stub)))
                 (or (string= pcomplete-stub ; Emacs 23
                              (substring buftext
                                         (max 0
                                              (- (length buftext)
                                                 (length pcomplete-stub)))))
                     (string= pcomplete-stub ; Emacs 24
                              (substring arg
                                         (max 0
                                              (- (length arg)
                                                 (length pcomplete-stub)))))))
        ;; Collect all possible completions for the stub. Note that
        ;; `candidates` may be a function, that's why we use
        ;; `all-completions`.
        (let* ((cnds (all-completions pcomplete-stub candidates))
               (bnds (completion-boundaries pcomplete-stub
                                            candidates
                                            nil
                                            ""))
               (skip (- (length pcomplete-stub) (car bnds))))
          ;; We replace the stub at the beginning of each candidate by
          ;; the real buffer content.
          (mapcar #'(lambda (cand) (concat buftext (substring cand skip)))
                  cnds))))))

(defvar ac-source-pcomplete
  '((candidates . ac-pcomplete)))


(defun user/auto-complete-init ()
  "Initialize auto-complete."
  (with-feature 'auto-complete-config
    ;; Load default configuration.
    (ac-config-default))

  (after-load 'diminish
    (diminish 'auto-complete-mode))

  (setq-default
   ;; Limit the number of candidates.
   ac-candidate-limit 20
   ;; Delay until narrowing completions.
   ac-delay 0.5
   ;; Automatically start completion after two characters.
   ac-auto-start 2
   ;; Use fuzzy matching.
   ac-fuzzy-enable t
   ac-use-fuzzy t
   ;; Automatically show menu after 400ms.
   ac-auto-show-menu 0.4
   ;; Allow normal navigation keys in menu.
   ac-use-menu-map t
   ;; Do not auto-expand common candidates.
   ac-expand-on-auto-complete nil
   ;; Show quick help popup after half a second.
   ac-use-quick-help t
   ac-quick-help-delay 0.5
   ;; Store the completion history in the cache directory.
   ac-comphist-file (path-join *user-cache-directory* "ac-comphist.dat"))

  ;; Install workaround for Flyspell
  (add-hook 'flymake-mode-hook 'ac-flyspell-workaround)

  ;; Set up auto-complete for lisp-interaction- and ielm-mode.
  (dolist (hook (list 'lisp-interaction-mode-hook
                      'ielm-mode-hook))
    (add-hook hook 'ac-emacs-lisp-mode-setup))

  ;;; (Bindings) ;;;
  (ac-set-trigger-key (user/get-key :code :try-complete))
  (setq-default
   ac-completing-map
   (let ((map (make-sparse-keymap)))
     ;; Expand on tab.
     (define-key map (user/get-key :code :try-complete) 'ac-expand-common)
     ;; Complete on enter.
     (define-key map (user/get-key :code :complete) 'ac-complete)
     ;; Bind configured auto complete key.
     (define-key map (user/get-key :code :auto-complete) 'auto-complete)
     ;; Scroll quick-help using M-n/p.
     (define-key map (kbd "C-M-n") 'ac-quick-help-scroll-down)
     (define-key map (kbd "C-M-p") 'ac-quick-help-scroll-up)
     map))

  ;; Enable auto-completion globally.
  (global-auto-complete-mode t))


(defun add-ac-sources (&rest sources)
  "Add SOURCES for auto-complete after it has been loaded."
  (after-load 'auto-complete
    (dolist (source sources)
      (if (boundp 'ac-sources)
          (add-to-list 'ac-sources source)
        (error "Declaration of ac-sources is missing!")))))


(defun add-ac-modes (&rest major-modes)
  "Add MAJOR-MODES for auto-complete after it has been loaded."
  (after-load 'auto-complete
    (dolist (mode major-modes)
      (if (boundp 'ac-modes)
          (add-to-list 'ac-modes mode)
        (error "Declaration of ac-modes is missing!")))))


(defun user/completion-init ()
  "Initialize automatic code completion."
  (setq
   ;; Do not fall back to complete if auto-complete is unavailable.
   tab-always-indent t)

  ;; Allow completion of acronyms and initialisms.
  (add-to-list 'completion-styles 'initials t)

  ;;; (Packages) ;;;
  (require-package '(:name auto-complete :after (user/auto-complete-init))))

(user/completion-init)


(provide 'ux/completion)
;;; completion.el ends here
