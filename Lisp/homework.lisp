;; Name:  Timothy Wilson
;; Class: CSC 345
;; Assignment: Homework 1
;; Description: To create and test the following functions purely recursively:
;; sum, my-replace, fibonacci, and fibonacci-TR.


;;;====================================================================================
;; Function:  sum
;; Called by: (sum n m)
;; Parameter: n - a number to add to the other parameter
;; Parameter: m - a number to add to the other parameter
;; Goal:      returns the sum of n and m using recursion and the 1+ and 1- functions,
;;            but not the general + function. The arguments n and m are integers,
;;            either zero, positive, or negative; if either argument is otherwise,
;;            the function returns nil.

(defun sum (n m)
  "returns the sum of n and m using recursion"
  (cond
    ;; Perform type checking
    ((not(integerp n)) nil)
    ((not(integerp m)) nil)
    ((equal m 0) n)
    ((> 0 m) (sum (1- n) (1+ m)))
    (t       (sum (1+ n) (1- m)))))

;;;====================================================================================
;; Function:  my-replace
;; Called by: (my-replace e1 e2 L)
;; Parameter: e1 - element to be replaced
;; Parameter: e2 - element to replace
;; Parameter: L  - the list to perform this operation on.
;; Goal:      returns the list L with all occurrences of element e1 replaced, at all
;;            levels within the list, with the element e2. The replacement should
;;            proceed even if the elements are themselves lists; that is , do a deep
;;            replace. Does not handle the case where any of the arguments are
;;            defective.

(defun my-replace (e1 e2 L)
  "returns the list L with all occurrences of element e1 replaced bythe element e2"
  (cond
    ((endp L) nil)
    ;; For single level
    ((equal(first L) e1) (cons e2 (my-replace e1 e2 (rest L))))
    ;; For nested level
    ((listp (first L))   (cons (my-replace e1 e2 (first L)) (my-replace e1 e2 (rest L))))
    (t                   (cons (first L) (my-replace e1 e2 (rest L))))))
	    
;;;====================================================================================
;; Function   fibonacci
;; Called by: (fibonacci-tr n)
;; Parameter: n - the nth number in the Fibonacci sequence.
;; Goal:      Returns the nth term in the Fibonacci sequence calculated by
;;            double recursion.
;;            Fibonacci follows the form: 1 1 2 3 5 8 13 21 34 55 89 ...

(defun fibonacci (n)
  "Returns the nth term in the Fibonacci sequence calculated by double recursion"
  (cond
    ((< n 0) nil)
    ((equal n 0) 0) 
    ((equal n 1) 1)
    (t(+ (fibonacci (1- n)) (fibonacci (- n 2))))))











;;;====================================================================================
;; Function   fibonacci-TR
;; Called by: (fibonacci-tr n)
;; Parameter: n - the nth number in the Fibonacci sequence.
;; Goal:      Returns the nth term in the Fibonacci sequence calculated by
;;            tail recursion.
;;            Fibonacci follows the form: 1 1 2 3 5 8 13 21 34 55 89 ...

(defun fibonacci-TR(n)
  "Returns the nth term in the Fibonacci sequence calculated by tail recursion"
    (labels (
	     (inner-fibonacci(x y n)
	       (cond ((equal n 0) x)
		     ;; Instead of calling double recursion, make each parameter
		     ;; an accumulator and decrement n by 1.
		     (t(inner-fibonacci(+ x y) x (1- n))))))
	;; Use an inner label so the user cannot mistakenly enter information to
        ;; break the calculation
        (inner-fibonacci 0 1 n)))

;;;====================================================================================
;;; END
