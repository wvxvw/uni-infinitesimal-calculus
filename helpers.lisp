(defun factorial (n)
  (loop :for i :upto n
     :for result := 1 :then (* result i)
     :finally (return result)))

(defun binom (n k)
  (/ (factorial n) (* (factorial k) (factorial (- n k)))))

(defun assignment-11.1.a (len)
  (loop :for i :upto len
     :collect (cons (expt 4 i) (binom (* 2 i) i))))

(defun assignment-11.1.b (len)
  (loop :for i :from 1 :upto len
     :collect (let* ((a (binom (* 2 i) i))
                     (b (floor (/ (* 2 (expt 4 (1- i))) (coerce i 'float))))
                     (c (floor (/ (expt 2 (1+ i)) (coerce i 'float))))
                     (d (/ (factorial (* 2 i)) (* (factorial i) (factorial i))))
                     (e (/ (* a (+ (* 4 i) 2)) (+ i 1))))
                (list a d e b c (- a b)))))
