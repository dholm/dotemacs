;; Stolen from https://github.com/purcell/emacs.d

(defun dholm/time-subtract-millis (b a)
  (* 1000.0 (float-time (time-subtract b a))))


(defvar dholm/require-times nil
    "A list of (FEATURE . LOAD-DURATION).
LOAD-DURATION is the time taken in milliseconds to load FEATURE.")


(defvar dholm/load-times nil
    "A list of (FILE . LOAD-DURATION).
LOAD-DURATION is the time taken in milliseconds to load FILE.")


(defadvice require
  (around build-require-times (feature &optional filename noerror) activate)
  "Note in `dholm/require-times' the time taken to require each feature."
  (let* ((already-loaded (memq feature features))
         (require-start-time (and (not already-loaded) (current-time))))
    (prog1
        ad-do-it
      (when (and (not already-loaded) (memq feature features))
        (add-to-list 'dholm/require-times
                     (cons feature
                           (dholm/time-subtract-millis (current-time)
                                                       require-start-time))
                     t)))))


(defadvice load
  (around build-load-times (file &optional noerror nomessage nosuffix must-suffix) activate)
  "Note in `dholm/load-times' the time taken to load each file."
  (let* ((load-start-time (current-time)))
    (prog1
        ad-do-it
      (add-to-list 'dholm/load-times
                     (cons file
                           (dholm/time-subtract-millis (current-time)
                                                       load-start-time))
                     t))))


(provide 'utilities/benchmarking)
