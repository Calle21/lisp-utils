(defun pattern-match (pat exp)
  (if (and (consp pat)
           (consp exp))
    (and (pattern-match (car pat) (car exp))
         (pattern-match (cdr pat) (cdr exp)))
    (or (eql pat exp)
        (eql pat _))))

