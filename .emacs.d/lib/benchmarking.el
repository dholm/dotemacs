;;; benchmarking --- benchmarks require and load statements
;;; Commentary:
;;;   Stolen from https://github.com/purcell/emacs.d
;;; Code:

(defun user/time-subtract-millis (b a)
  "Calculate the number of milliseconds that have elapsed between B and A."
  (* 1000.0 (float-time (time-subtract b a))))


(defvar user/require-times (make-hash-table :test 'equal)
    "A hash table of (FEATURE . LOAD-DURATION).
LOAD-DURATION is the time taken in milliseconds to load FEATURE.")


(defvar user/load-times (make-hash-table :test 'equal)
    "A hash-table of (FILE . LOAD-DURATION).
LOAD-DURATION is the time taken in milliseconds to load FILE.")


(defun user/show-require-times ()
  "Show the benchmark for require."
  (interactive)
  (user/show-times user/require-times "*require times*"))

(defun user/show-load-times ()
  "Show the benchmark for load."
  (interactive)
  (user/show-times user/load-times "*load times*"))


(defmacro user/show-times (hash-table name)
  "Show the benchmark for the specified HASH-TABLE in buffer NAME."
  `(with-output-to-temp-buffer ,name
     (do-hash-table-sorted-by-value (val ,hash-table)
       (princ (format "%s %d\n" val (gethash val ,hash-table))))
     (switch-to-buffer ,name)))


(defadvice require
  (around build-require-times (feature &optional filename noerror) activate)
  "Note in `user/require-times' the time taken to require each feature."
  (let* ((already-loaded (memq feature features))
         (require-start-time (and (not already-loaded) (current-time))))
    (prog1
        ad-do-it
      (when (and (not already-loaded) (memq feature features))
        (puthash feature (user/time-subtract-millis (current-time)
                                                     require-start-time)
                 user/require-times)))))


(defadvice load
  (around build-load-times (file &optional noerror nomessage nosuffix must-suffix) activate)
  "Note in `user/load-times' the time taken to load each file."
  (let* ((load-start-time (current-time)))
    (prog1
        ad-do-it
      (progn
        (unless (eq (gethash file user/load-times) nil)
          (message (format "Loading %s which has already been loaded!" file)))
        (puthash file (user/time-subtract-millis (current-time)
                                                  load-start-time)
                 user/load-times)))))


(provide 'lib/benchmarking)
;;; benchmarking.el ends here
