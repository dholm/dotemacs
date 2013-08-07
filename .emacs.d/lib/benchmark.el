;;; benchmark.el --- benchmarks require and load statements
;;; Commentary:
;;;   Original version stolen from https://github.com/purcell/emacs.d
;;; Code:

(defun benchmark/time-subtract-millis (b a)
  "Calculate the number of milliseconds that have elapsed between B and A."
  (* 1000.0 (float-time (time-subtract b a))))


(defvar benchmark/require-times (make-hash-table :test 'equal)
  "A hash table of (FEATURE . LOAD-DURATION).
LOAD-DURATION is the time taken in milliseconds to load FEATURE.")


(defvar benchmark/load-times (make-hash-table :test 'equal)
  "A hash-table of (FILE . LOAD-DURATION).
LOAD-DURATION is the time taken in milliseconds to load FILE.")


(defun benchmark/show-require-times ()
  "Show the benchmark for require."
  (interactive)
  (benchmark/show-times benchmark/require-times "*require times*"))

(defun benchmark/show-load-times ()
  "Show the benchmark for load."
  (interactive)
  (benchmark/show-times benchmark/load-times "*load times*"))


(defmacro benchmark/show-times (hash-table name)
  "Show the benchmark for the specified HASH-TABLE in buffer NAME."
  `(with-output-to-temp-buffer ,name
     (do-hash-table-sorted-by-value (val ,hash-table)
       (princ (format "%s %d\n" val (gethash val ,hash-table))))
     (switch-to-buffer ,name)))


;;;###autoload
(defun benchmark/install ()
  "Install benchmark support in Emacs."
  (defadvice require
    (around build-require-times (feature &optional filename noerror) activate)
    "Note in `benchmark/require-times' the time taken to require each feature."
    (let* ((already-loaded (memq feature features))
           (require-start-time (and (not already-loaded) (current-time))))
      (prog1
          ad-do-it
        (when (and (not already-loaded) (memq feature features))
          (puthash feature (benchmark/time-subtract-millis (current-time)
                                                           require-start-time)
                   benchmark/require-times)))))

  (defadvice load
    (around build-load-times (file &optional noerror nomessage nosuffix must-suffix) activate)
    "Note in `benchmark/load-times' the time taken to load each file."
    (let* ((load-start-time (current-time)))
      (prog1
          ad-do-it
        (progn
          (unless (eq (gethash file benchmark/load-times) nil)
            (message (format "Loading %s which has already been loaded!" file)))
          (puthash file (benchmark/time-subtract-millis (current-time)
                                                        load-start-time)
                   benchmark/load-times))))))


(provide 'lib/benchmark)
;;; benchmark.el ends here
