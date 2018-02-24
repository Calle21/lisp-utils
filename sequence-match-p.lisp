(defun sequence-match-p (obj1 obj2 sequence)
  (labels ((sequence-match-p-lst (obj1 obj2 lst)
             (if (null lst)
               t
               (let ((elem (car lst)))
                 (cond
                   ((eql elem obj1) t)
                   ((eql elem obj2) nil)
                   (t (sequence-match-p-lst obj1 obj2 (cdr lst)))))))
           (sequence-match-p-arr (obj1 obj2 arr)
             (dotimes (i (length arr) t)
               (let ((elem (aref arr i)))
                 (cond
                   ((eql elem obj1) (return t))
                   ((eql elem obj2) (return nil)))))))
    (typecase sequence
      (list (sequence-match-p-lst obj1 obj2 sequence))
      (array (sequence-match-p-arr obj1 obj2 sequence)))))
