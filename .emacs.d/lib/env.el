(defun getenv-or (env value)
  "Fetch the value of env or if it is not set return value"
  (if (getenv env)
      (getenv env)
    value))


(provide 'lib/env)
