;;;;;(sum n m)
;;;; returns the sum of n and m using recursion and the 1+ and 1- functions, but not
;;;; the general + function. The arguments n and m are integers, either zero, positive, or negative;
;;;; if either argument is otherwise, the function returns nil.

(defun sum (n m)
  "returns the sum of n and m using recursion"
  (cond
    ((not(integerp n)) nil)
    ((not(integerp m)) nil)
    ((equal m 0) n)
    ((> 0 m) (sum (1- n) (1+ m)))
    (t       (sum (1+ n) (1- m)))))

;;;;;(my-replace e1 e2 L)
;;;; returns the list L with all occurrences of element e1 replaced, at all levels within the
;;;; list, with the element e2. The replacement should proceed even if the elements are themselves
;;;; lists; that is , do a deep replace. You do not need to handle the case where any of the
;;;; arguments are defective.

(defun my-replace (e1 e2 L)
  "returns the list L with all occurrences of element e1 replaced bythe element e2"
  (cond
    ((endp L) nil)
    ;; For single level
    ((equal(first L) e1) (cons e2 (my-replace e1 e2 (rest L))))
    ;; For nested level
    ((listp (first L))   (cons (my-replace e1 e2 (first L)) (my-replace e1 e2 (rest L))))
    ;; For all else
    (t                   (cons (first L) (my-replace e1 e2 (rest L))))))
	    


;;;;;(fibonacci-tr n)
;;;; Implement this function in the double-recursion way.
;;;; Fibonacci follows the form: 1 1 2 3 5 8 13 21 34 55 89 ...

(defun fibonacci (n)
  "Returns the nth term in the Fibonacci sequence by double recursion"
  (cond((equal n 0) 0)
       ((equal n 1) 1)
       (t(+ (fibonacci (1- n)) (fibonacci (- n 2))))))

;;;;;(fibonacci-tr n)
;;;; Implement this function in the tail-recursive way.
;;;; Fibonacci follows the form: 1 1 2 3 5 8 13 21 34 55 89 ...

(defun fibonacci-tr(n)
  "Returns the nth term in the Fibonacci sequence by tail recursion"
  (let(
    (x 0)
    (y 1))
    (labels (
	     (inner-fibonacci(x y n)
	       (cond ((equal n 0) x)
		     (t(inner-fibonacci(+ x y) x (1- n))))))
      ;;run funct
      (inner-fibonacci x y n))))
