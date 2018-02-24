(defpackage :primes
            (:use :common-lisp)
	    (:export :make-primes :make-prime-table :factor :make-primes-fast))

(in-package :primes)

(defstruct layer
  (rhythm nil :type fixnum)
  (current nil :type fixnum))

(defun make-primes (n)
  (labels ((update-layers (lst)
		 (mapc #'(lambda (layer)
		 	   (setf (layer-current layer)
				 (the fixnum (mod (1- (layer-current layer))
					 	      (layer-rhythm layer)))))
		       lst))
	   (primep (layer)
  	      (not (zerop (layer-current layer)))))
    (do ((primes (make-array n :element-type 'fixnum))
         (layers nil (update-layers layers))
         (num 2 (1+ num))
         (i 0))
       ((= n i) primes)
      (declare (fixnum num i))
      (if (every #'primep layers)
        (setf
  	  (aref primes i) num
	  i (1+ i)
	  layers (nconc layers (list (make-layer :rhythm num
					         :current 0))))))))

(defun make-prime-table (ceiling)
  (let ((thearray (make-array (1+ ceiling) :element-type 'boolean :initial-element t))) ; Ändra till bit
    (setf (aref thearray 0) nil
	  (aref thearray 1) nil)
    (do ((i 2 (1+ i)))
       ((= i (1+ ceiling)) thearray)
      (if (aref thearray i)
	(do ((i2 (* i 2) (+ i2 i)))
	  ((>= i2 (1+ ceiling)))
	  (setf (aref thearray i2) nil))))))

(defun make-primes-fast (n)
  (declare (fixnum n))
  (if (<= n 0)
    #()
    (let ((primes (make-array n :element-type 'fixnum)))
      (setf (aref primes 0) 2)
      (do ((i 1)
           (prime? 3 (+ prime? 2)))
         ((= i n) primes)
        (declare (fixnum i))
        (if (do* ((j 0 (1+ j))
                  (p (aref primes j)
                     (aref primes j)))
                (())
              (cond
                ((zerop (rem prime? p))
                 (return nil))
                ((< prime? (* p p))
                 (return t))))
          (setf (aref primes i) prime?
                             i (1+ i)))))))

(let ((primes (make-primes-fast 2000)))
  (defun factor (num)
    (declare (integer num))
    (cond
      ((zerop num)
       (list 0))
      ((= num 1)
       nil)
      ((minusp num)
       (cons -1 (factor (* num -1))))
      (t
	(dotimes (i 2000 (error "Number too large to be factorized"))
	  (let ((prime (aref primes i)))
	    (multiple-value-bind (quot rest) (truncate num prime)
	      (cond
		((zerop rest)
		 (return (cons prime (factor quot))))
		((< num (* prime prime))
		 (return (list num)))))))))))

