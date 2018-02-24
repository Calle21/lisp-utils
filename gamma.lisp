(defmacro super (name let-list &body body)
 `(labels ((,name ,(mapcar #'(lambda (node) (typecase node (symbol node) (cons (car node))))
			   let-list)
		  ,@body))
    (,name ,@(mapcar #'(lambda (node) (typecase node (symbol nil) (cons (second node))))
		     let-list))))

(defmacro gamma (let-list &body body)
 `(super rec ,let-list ,@body))

