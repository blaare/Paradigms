;; Name:        Timothy Wilson
;; Class:       CSC 345
;; Assignment:  Project 1
;; Description: To implement the function integrate, which integrates certain symbolic
;;              expressions with respect to a variable. If lower and upper limits are
;;              given, the definite integral is constructed and then evaluated, and if
;;              they are omitted, the indefinite integral is constructed.


;;;==========================================================================
;;; SYMBOLS
;;  The symbols are defined here for use in the functions of this file.

(defconstant variable-symbols '(U V W X Y Z))
(defconstant negative-symbol  '-)
(defconstant sum-symbol       '+)
(defconstant product-symbol   '*)
(defconstant quotient-symbol  '/)
(defconstant exponent-symbol  'EXPT)

;;;==========================================================================
;;; SELECTORS -- OPERATORS
;;  The OPERATOR functions are used to return the symbol
;;  of the function F. 

(defun negative-operator (F) (first F))
(defun sum-operator      (F) (first F))
(defun product-operator  (F) (first F))
(defun quotient-operator (F) (first F))

;;;==========================================================================
;;; SELECTORS -- OPERANDS
;;  The OPERANDS functions are used to return the value(s)
;;  of the function F.

;;;==========================================================================
;; NEGATIVE Operand
;; Goal: to return the operand(s) of some function F.

(defun negative-operand      (F)
  (cond
    ((equal (length F) 2) (second F))
    (t (rest F))))

;;;==========================================================================
;; SUM Operands

(defun sum-operand-1         (F) (second F))
(defun sum-operand-2         (F) (third F))

;;;==========================================================================
;; SUBTRACTION Operands

(defun subtraction-operand-1 (F) (second F))
(defun subtraction-operand-2 (F) (third F))

;;;==========================================================================
;; POWER Operands

(defun power-operand-1       (F) (second F))
(defun power-operand-2       (F) (third F))

;;;==========================================================================
;; PRODUCT Operands

(defun product-operand-1       (F) (second F))
(defun product-operand-2       (F) (third F))


;;;==========================================================================
;;; PREDICATES

;;;==========================================================================
;; Function   variable-p
;; Goal:      to determine if F is a variable, according to variable-symbols
;; Parameter: F - Some value
;; Return:    nil or t

(defun variable-p (F)
  (member F variable-symbols))

;;;==========================================================================
;; Function   negative-p
;; Goal:      returns t if F is only length 2,
;;            the first symbol is -,
;;            and the second symbol is not -
;; Parameter: F - Some list/function
;; Return:    nil or t

(defun negative-p (F)
  (cond
    ((endp F) nil)
    ((and (equal (length F) 2)
          (equal (negative-operator F) negative-symbol)
	  (not (equal (negative-operand F) negative-symbol))) t)
    (t(negative-p (rest F)))))
    
;;;==========================================================================
;;  Function   sum-p
;;  Goal:      returns t if the first of F is +
;;  Parameter: F - some list/function
;;  Return:    nil or t 

(defun sum-p (F)
  (eq (first F) sum-symbol))

;;;==========================================================================
;;; Function   subtract-p
;;  Goal:      t if the first of F is - and the length is 3
;;  Parameter: F - some list/function
;;  Return:    nil or t

(defun subtraction-p(F)
  (and
   (equal (length F ) 3)
   (equal (first F) negative-symbol) 
   (not (equal (subtraction-operand-1 F) negative-symbol))
   (not (equal (subtraction-operand-2 F) negative-symbol))))

;;;==========================================================================
;;; Function   product-p
;;  Goal:      returns t if the first element is *
;;  Parameter: F - some list/function
;;  Return:    nil or t

(defun product-p (F)
  (eq (first F) product-symbol))

;;;==========================================================================
;;; Function   quotient-p
;;  Goal:      returns t if the first element is /
;;  Parameter: F - some list/function
;;  Return:    nil or t
(defun quotient-p (F)
  
  (eq (first F) quotient-symbol))

;;;==========================================================================
;;  Function   exponent-p
;;  Goal:      returns t if the elements follow the form (expt x 4)
;;  Parameter: F - some list/function
;;  Return:    nil or t

(defun power-p (F)
  (eq (first F) exponent-symbol))



;;;==========================================================================
;;; CONSTRUCTORS

;;;==========================================================================
;; Function   make-variable
;; Goal:      to make a variable out of V
;; Parameter: V - Some variable
;; Return:    the variable V

(defun make-variable (V) V)

;;;==========================================================================
;; Function   make-sum
;; Goal:      to make an addition list out of F and G
;; Parameter: F - Some value, integer or otherwise
;; Parameter: G - Some value, integer or otherwise
;; Return:    the calculated version of (+ <F> <G>)

(defun make-sum (F G)
  (cond ((eq 0 F) G)                            ; G+0=G
	((eq 0 G) F)                            ; F+0=F
	((equal (make-negative F) G) 0)
	((and (numberp F) (numberp G)) (+ F G)) ; if they're numbers, add them
	(t (list sum-symbol F G))))             ; return: (+ F G)

;;;==========================================================================
;; Function   make-subtraction
;; Goal:      to make a subtraction list out of F and G
;; Parameter: F - Some value, integer or otherwise
;; Parameter: G - Some value, integer or otherwise
;; Return:    the calculated version of (- <F> <G>)

(defun make-subtraction (F G)
  (cond ((equal 0 F) (make-negative G))
	((equal 0 G) F)
	((equal F G) 0)
	((and (numberp F) (numberp G)) (- F G))
	(t (list negative-symbol F G))))

;;;==========================================================================
;; Function   make-product
;; Goal:      to make a product list out of F and G
;; Parameter: F - Some value, integer or otherwise
;; Parameter: G - Some value, integer or otherwise
;; Return:    the calculated version of (* <F> <G>)

(defun make-product (F G)
  (cond ((eq 0 F) 0)                                         ; F*0=0
	((eq 0 G) 0)                                         ; G*0=0
	((and (numberp F) (numberp G)) (* F G))              ; if they're numbers, multiply
	((equal F G) (make-power F 2))                       ; if they're variables, simplify
	(t (list product-symbol F G))))                      ; return: (* F G)

;;;==========================================================================
;; Function   make-quotient
;; Goal:      to make a quotient out of F and G
;; Parameter: F - Some value, integer or otherwise
;; Parameter: G - Some value, integer or otherwise
;; Return:    the calculated version of (/ <F> <G>)

(defun make-quotient(F G)
  (cond ((eq 0 F) 0)
	((eq 0 G) nil)
	((and (numberp F) (numberp G)) (/ F G))
	((equal F G) 1)
	(t (list quotient-symbol F G))))

;;;==========================================================================
;;; Function   make-power
;;  Goal:      to turn expt x 2 into (EXPT X 2)
;;  Parameter: F - Some value, integer or otherwise
;;  Parameter: G - Some value, integer or otherwise
;;  Return:    the calculated version of (EXPT <F> <G>)

(defun make-power(F G)
  (cond ((eq 0 F) 0)
	((eq 0 G) 1)
	((and (numberp F) (numberp G)) (expt F G))
	(t (list exponent-symbol F G))))

;;;==========================================================================
;;  Function   make-negative
;;  Goal:      to turn (- x) into (- (x)) and (- 5) into -5
;;             and to turn (- - x) into (-(-x))
;;  Parameter: F - Some function to make negative
;;  Return:    the calculated negative version of F

(defun make-negative(F)
  (cond ((numberp F) (list negative-symbol F))
	((variable-p F) (list negative-symbol F))
        ((negative-p F) (negative-operand F))
        (t (list negative-symbol F))))

;;;==========================================================================
;;  Function   make-log
;;  Goal:      To handle making a LOG function
;;  Parameter: V - The variable for the LOG function
;;  Return:    (LOG <V>)

(defun make-log(V)
  (list 'log V))



;;;==========================================================================
;;;INTEGRATION: CORE

;;;==========================================================================
;; Function:  integrate
;; Goal:      to evaluate the indefinite/definite integral of the function F
;;            with respect to V from low bound to high bound.
;; Parameter: F - Some function
;; Parameter: V - Some variable
;; Parameter: lo - the low value of the indefinite integral
;; Parameter: hi - the high value of the indefinite integral
;; Return:    The integral of F, definite or indefinite.

(defun integrate (F V &optional lo hi)
  "Computes the integral of with respect to V"
  (def-integral (indef-integral F V) lo hi))

;;;==========================================================================
;; Function:  indef-integral
;; Goal:      to evaluate the indefinite integral of the function F with respect
;;            to V.
;; Parameter: F - Some function
;; Parameter: V - Some variable
;; Return:    The indefinite integral of F.

(defun indef-integral (F V)
  "Computes the indefinite integral"
  (cond
    ((numberp F)       (make-product F V))                             ;; Handle integers
    
    ((variable-p F)    (make-product                                   ;; Handle X
			(make-quotient 1 2)
			(make-power F 2))) ;; Handle X
    
    ((sum-p F)         (make-sum
		        (indef-integral (sum-operand-1 F) V)           ;; Handle f(x) + g(x)
		        (indef-integral (sum-operand-2 F) V)))
    
    ((subtraction-p F) (make-subtraction                               ;; Handle f(x) - g(x)
			(indef-integral (subtraction-operand-1 F) V)
			(indef-integral (subtraction-operand-2 F) V)))
    
    ((product-p F)     (cond                                           ;; Handle f(x) * f(x)
			 ((numberp (product-operand-1 F))
			  (cond                                        ;; Handle n *(n|x)
			    ((numberp (product-operand-2  F)) (indef-integral (make-product      
							       (product-operand-1 F)
							       (product-operand-2 F)) V))
			    (t(make-product (product-operand-1 F)
					    (indef-integral (product-operand-2 F) V)))))
			 (t
			  (cond                                        ;; Handle x *(x|n)
			    ((variable-p (product-operand-2 F)) (indef-integral (make-product
								 (product-operand-1 F)
								 (product-operand-2 F)) V))
			    (t(make-product (product-operand-2 F)
					    (indef-integral (product-operand-1 F) V)))))))
							   
			  
    
    ((negative-p F)    (make-negative                                  ;; Handle - f(x)
			(indef-integral (negative-operand F) V)))       
    ((power-p F) (cond                                                 ;; Handle expt x n
                  ((equal (power-operand-2 F) -1)                      ;; Handle expt x -1
	               (make-log V))                                        
                  (t(make-product (make-quotient 1
			(1+ (power-operand-2 F)))
		       (make-power
			(power-operand-1 F)
			(1+ (power-operand-2 F)))))))
    (t nil)
    ))
	
;;;==========================================================================
;; Function def-integral
;; Called by: (def-integral F lo hi)
;; Parameter: F - the function to evaluate the definite integral on
;; Parameter: lo - the low value of the definite integral
;; Parameter: hi - the high value of the definite integral
;; Goal:      return the definite integral of the function based on the low and high values.

(defun def-integral (F lo hi)
  "Computes the definite integral of F with respect to lo and hi values"
  (cond
        ;; if values supplied for lo and hi
        ;; return the definite integral
    ((and lo hi) (- (eval (my-replace F hi)) (eval (my-replace F lo))))
        ;; else return the indefinite integral
    (t F)))                      



;;;==========================================================================
;;;INTEGRATION: MISC

;;;==========================================================================
;; Function:  my-replace
;; Called by: (my-replace L val)
;; Parameter: L  - the list to perform this operation on.
;; Parameter: val - the value to replace the variables
;; Goal:      returns the list L with all occurrences of variables replaced with val.


(defun my-replace (L val)
  "returns the list L with all occurrences of element e1 replaced by the element e2"
  (cond
    ;; Handle single-value integrals.
    ((variable-p L) val)
    ((numberp L) L)
    (t
     (cond
       ((endp L) nil)
       ((variable-p (first L)) (cons val (my-replace (rest L) val)))
       ;; For nested level
       ((listp (first L))   (cons (my-replace (first L) val) (my-replace (rest L) val)))
       (t                   (cons (first L) (my-replace (rest L) val)))
    ))))



