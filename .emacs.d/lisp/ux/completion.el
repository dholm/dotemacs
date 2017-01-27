;;; completion.el --- Configures automatic code completion
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'cl))


(defun user--auto-complete-mode-hook ()
  "Auto complete mode hook."
  (user/complete-at-point-install))


(defun user--company-mode-hook ()
  "Company mode hook."
  (user/complete-at-point-install)

  (when (display-graphic-p)
    (with-feature 'company-quickhelp
      ;; Enable help popups.
      (company-quickhelp-mode t)))

  ;;; (Bindings) ;;;
  (when (current-local-map)
    (define-key (current-local-map) (user/get-key :code :auto-complete) 'company-complete)))


(defun user/auto-complete-p ()
  "True if auto-complete should be used."
  (and (boundp 'global-auto-complete-mode)
       global-auto-complete-mode))


(defun user/company-mode-p ()
  "True if auto-complete should be used."
  (and (boundp 'global-company-mode)
       global-company-mode))


(defun user/complete-at-point-install ()
  "Install completion backend in complete-at-point."
  (setq
   ;; Insert completion backend into `completion-at-point'.
   completion-at-point-functions
   (cons 'user/complete-at-point
         (remove 'user/complete-at-point completion-at-point-functions))))


(defun user/company-check-expansion ()
  "Check if expression can be expanded by company."
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "->") t nil)))))


(defun user/complete-at-point ()
  "Complete thing at point using completion backend."
  (cond
   ((minibufferp) (minibuffer-complete))
   ((and (boundp 'auto-complete-mode) auto-complete-mode)
    #'auto-complete)
   ((and (boundp 'company-mode) company-mode (user/company-check-expansion))
    #'company-complete-common)
   (t #'indent-for-tab-command)))


(defun ac-pcomplete ()
  "Auto-complete source for pcomplete."
  ;; eshell uses `insert-and-inherit' to insert a \t if no completion
  ;; can be found, but this must not happen as auto-complete source
  (cl-flet ((insert-and-inherit (&rest args)))
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


(defun add-ac-sources (&rest sources)
  "Add SOURCES for auto-complete after it has been loaded."
  (when (user/auto-complete-p)
    (dolist (source sources)
      (if (boundp 'ac-sources)
          (add-to-list 'ac-sources source)
        (error "Declaration of ac-sources is missing!")))))


(defun add-ac-modes (&rest major-modes)
  "Add MAJOR-MODES for auto-complete after it has been loaded."
  (when (user/auto-complete-p)
    (dolist (mode major-modes)
      (if (boundp 'ac-modes)
          (add-to-list 'ac-modes mode)
        (error "Declaration of ac-modes is missing!")))))


(defun user--auto-complete-config ()
  "Initialize auto-complete."
  (with-feature 'auto-complete-config
    ;; Load default configuration.
    (ac-config-default)
    ;; Don't forcibly enable auto-complete.
    (global-auto-complete-mode -1))

  (setq-default
   ;; Limit the number of candidates.
   ac-candidate-limit 40
   ;; Delay until narrowing completions.
   ac-delay 0.5
   ;; Do not trigger completion automatically.
   ac-auto-start nil
   ;; Use fuzzy matching.
   ac-fuzzy-enable t
   ac-use-fuzzy t
   ;; Do not pop up menu automatically.
   ac-auto-show-menu nil
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

  ;;; (Hooks) ;;;
  (add-hook 'auto-complete-mode-hook 'user--auto-complete-mode-hook)

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
     map)))


(defun company-complete-common-or-selection ()
  "Insert the common part of all candidates, or the selection."
  (interactive)
  (when (company-manual-begin)
    (let ((tick (buffer-chars-modified-tick)))
      (call-interactively 'company-complete-common)
      (when (eq tick (buffer-chars-modified-tick))
        (let ((company-selection-wrap-around t))
          (call-interactively 'company-complete-selection))))))



(defun add-company-sources (&rest sources)
  "Add SOURCES for company completion in current mode, if it has been loaded."
  (when (user/company-mode-p)
    (when (boundp 'company-backends)
      (setq-local
       company-backends
       (append sources company-backends)))))


(defun user--company-mode-config ()
  "Initialize company mode."
  (setq-default
   ;; Do not trigger completion automatically.
   company-idle-delay nil
   ;; Complete immediately.
   company-minimum-prefix-length 0
   ;; Disable completion in certain modes.
   company-global-modes '(not git-commit-mode)
   ;; Show commonly used matches first.
   company-transformers '(company-sort-by-occurrence)
   ;; Align annotations to the right of tooltip.
   company-tooltip-align-annotations t
   ;; Complete even outside of code.
   company-dabbrev-code-everywhere t
   ;; Active company frontends.
   company-frontends
   '(company-pseudo-tooltip-unless-just-one-frontend
     company-preview-frontend
     company-echo-metadata-frontend)
   ;; Show quick help popup after half a second.
   company-quickhelp-delay 0.5)

  ;;; (Hooks) ;;;
  (add-hook 'company-mode-hook 'user--company-mode-hook)

  ;;; (Bindings) ;;;
  (after-load 'company
    (define-key company-active-map
      (user/get-key :code :complete) 'company-complete-selection)
    (define-key company-active-map
      (user/get-key :code :try-complete) 'company-complete-common-or-selection)
    (define-key company-active-map
      (user/get-key :basic :forward-line) 'company-select-next)
    (define-key company-active-map
      (user/get-key :basic :backward-line) 'company-select-previous))

  ;; Enable company completion globally.
  (global-company-mode t)
  (after-load 'company
    (with-feature 'company-flx
      ;; Enable fuzzy matching.
      (company-flx-mode t))))


(defun user--completion-config ()
  "Initialize automatic code completion."
  (setq
   ;; Activate completion on tab.
   tab-always-indent 'complete
   ;; Cycle completions in minibuffer below a certain threshold.
   completion-cycle-threshold 5)

  ;; Allow completion of acronyms and initialisms.
  (add-to-list 'completion-styles 'initials t)

  ;;; (Packages) ;;;
  (req-package auto-complete
    :loader :el-get
    :diminish auto-complete-mode
    :config (user--auto-complete-config))
  (req-package company
    :diminish company-mode
    :config (user--company-mode-config))
  (req-package company-flx)
  (req-package company-quickhelp))

(user--completion-config)


(provide 'ux/completion)
;;; completion.el ends here
