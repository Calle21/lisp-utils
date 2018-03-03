(labels ((rec (ls)
            (declare (special *gens* *syms*))
            (if (null ls)
              nil
              (let ((g (gensym))
                    (s (pop ls)))
                (push g *gens*)
                (push s *syms*)
                (if ls
                  (cons (list g (pop ls))
                        (rec ls))
                  (error "Odd number of arguments given to psetq-"))))))
  (defmacro psetq- (&rest args)
    (let (*gens* *syms*)
      (declare (special *gens* *syms*))
      (let ((lets (rec args)))
        `(let ,lets
           (setq ,@(mapcan #'(lambda (g s)
                               `(,s ,g))
                           *gens*
                           *syms*))
           nil)))))

