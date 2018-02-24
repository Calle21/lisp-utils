(defun mapatom (fn first &rest rest)
  (cond
    ((null first)
     nil)
    ((atom first)
     (apply fn first rest))
    (t ; meant to be a list
     (cons (apply #'mapatom fn (car first) (mapcar #'car rest))
           (apply #'mapatom fn (cdr first) (mapcar #'cdr rest))))))

