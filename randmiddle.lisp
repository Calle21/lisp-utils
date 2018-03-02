(defun randmiddle (range)
  (declare (integer range))
  (let ((middle (truncate (/ range 2))))
    (let ((rand1 (random range))
          (rand2 (random middle)))
      (let ((distance-from-middle (abs (- middle rand1))))
        (if (> rand2 distance-from-middle)
          rand1
          (randmiddle range))))))
