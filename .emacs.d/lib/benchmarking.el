;;; benchmarking --- benchmarks require and load statements
;;; Commentary:
;;;   Stolen from https://github.com/purcell/emacs.d
;;; Code:

(defun dholm/time-subtract-millis (b a)
  "Calculate the number of milliseconds that have elapsed between B and A."
  (* 1000.0 (float-time (time-subtract b a))))


(defvar dholm/require-times (make-hash-table :test 'equal)
    "A hash table of (FEATURE . LOAD-DURATION).
LOAD-DURATION is the time taken in milliseconds to load FEATURE.")


(defvar dholm/load-times (make-hash-table :test 'equal)
    "A hash-table of (FILE . LOAD-DURATION).
LOAD-DURATION is the time taken in milliseconds to load FILE.")


(defun dholm/show-require-times ()
  "Show the benchmark for require."
  (interactive)
  (dholm/show-times dholm/require-times "*require times*"))

(defun dholm/show-load-times ()
  "Show the benchmark for load."
  (interactive)
  (dholm/show-times dholm/load-times "*load times*"))


(defmacro dholm/show-times (hash-table name)
  "Show the benchmark for the specified HASH-TABLE in buffer NAME."
  `(with-output-to-temp-buffer ,name
     (do-hash-table-sorted-by-value (val ,hash-table)
       (princ (format "%s %d\n" val (gethash val ,hash-table))))
     (switch-to-buffer ,name)))


(defadvice require
  (around build-require-times (feature &optional filename noerror) activate)
  "Note in `dholm/require-times' the time taken to require each feature."
  (let* ((already-loaded (memq feature features))
         (require-start-time (and (not already-loaded) (current-time))))
    (prog1
        ad-do-it
      (when (and (not already-loaded) (memq feature features))
        (puthash feature (dholm/time-subtract-millis (current-time)
                                                     require-start-time)
                 dholm/require-times)))))


(defadvice load
  (around build-load-times (file &optional noerror nomessage nosuffix must-suffix) activate)
  "Note in `dholm/load-times' the time taken to load each file."
  (let* ((load-start-time (current-time)))
    (prog1
        ad-do-it
      (progn
        (unless (eq (gethash file dholm/load-times) nil)
          (message (format "Loading %s which has already been loaded!" file)))
        (puthash file (dholm/time-subtract-millis (current-time)
                                                  load-start-time)
                 dholm/load-times)))))


(provide 'lib/benchmarking)
;;; benchmarking.el ends here
