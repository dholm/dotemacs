(defmacro try-eval (fn &optional finally)
  "Safely evaluate expression fn and run finally after"
  `(unwind-protect
       (let (retval)
         (condition-case ex
             (setq retval (progn ,fn))
           ('error
            (message (format "Caught exception: [%s]" ex))
            (setq retval (cons 'exception (list ex)))))
         retval)
     ,@finally))


(provide 'lib/utils)
