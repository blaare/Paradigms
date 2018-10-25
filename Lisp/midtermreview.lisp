
(defun summation (n1 n2)
  (cond ((eq n2 0) n1)
	((< n2 0) (summation (1+ n1) (1+ n2)))
	(t(summation (1+ n1) (1- n2)))))

(defun deep-reverse (L)
  (cond
    ((endp L) nil)
    ((listp (first L))(append(deep-reverse(rest L)) (list(deep-reverse(first L)))))
    (t               (append(deep-reverse(rest L)) (list(first L))))))

(defun factorial (n)
  (labels (
	   (inner-factorial(a acc)
			  (cond
			    ((eq a 1) acc)
			    (t(inner-factorial(1- a) (* a acc))))))
    (cond
      ((< n 0) nil)
      ((eq n 0) 1)
      (t(inner-factorial n 1)))))
