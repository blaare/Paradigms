;;;==========================================================================
;;; SYMBOLS
(defconstant variable-symbols '(U V W X Y Z))
(defconstant negative-symbol  '-)
(defconstant sum-symbol       '+)
(defconstant product-symbol   '*)
(defconstant quotient-symbol  '/)
(defconstant exponent-symbol  'EXPT)

;;;==========================================================================
;;; SELECTORS -- OPERATORS
(defun negative-operator (F) (first F))
(defun sum-operator      (F) (first F))
(defun product-operator  (F) (first F))
(defun quotient-operator (F) (first F))

;;; SELECTORS -- OPERANDS
(defun negative-operand      (F)
  (cond
    ((equal (length F) 2) (second F))
    (t (rest F))))
(defun sum-operand-1         (F) (second F))
(defun sum-operand-2         (F) (third F))
(defun subtraction-operand-1 (F) (second F))
(defun subtraction-operand-2 (F) (third F))
(defun power-operand-1       (F) (second F))
(defun power-operand-2       (F) (third F))


;;;==========================================================================
;;; PREDICATES

(defun variable-p (F)
  (member F variable-symbols))

;;; Function negative-p
;;  returns t if F is -
(defun negative-p (F)
  (cond
    ((endp F) nil)
    ((and (equal (length F) 2)
          (equal (negative-operator F) negative-symbol)
	  (not (equal (negative-operand F) negative-symbol))) t)
    (t(negative-p (rest F)))))
    

;;; Function sum-p
;;  returns t if the F is +
(defun sum-p (F)
  (eq (first F) sum-symbol))

;;; Function subtract-p
;;  returns t if the first of F is - and the length is 3
(defun subtraction-p(F)
  (and
   (equal (length F ) 3)
   (equal (first F) negative-symbol) 
   (not (equal (subtraction-operand-1 F) negative-symbol))
   (not (equal (subtraction-operand-2 F) negative-symbol))))

;;; Function product-p
;;  returns t if the first element is *
(defun product-p (F)
  (eq (first F) product-symbol))

;;; Function quotient-p
;;  returns t if the first element is /
(defun quotient-p (F)
  (eq (first F) quotient-symbol))

;;; Function exponent-p
;;  returns t if the elements follow the form (expt x 4)
(defun power-p (F)
  (eq (first F) exponent-symbol))

;;;==========================================================================
;;; CONSTRUCTORS
(defun make-variable (V) V)
;;; NOTE that this function, whcih seems 'trivial', is included
;;; for the completeness of the abstraction from the way the math
;;; functions are represented in Lisp

(defun make-sum (F G)
  (cond ((eq 0 F) G)                            ; G+0=G
	((eq 0 G) F)                            ; F+0=F
	((equal (make-negative F) G) 0)
	((and (numberp F) (numberp G)) (+ F G)) ; if they're numbers, add them
	(t (list sum-symbol F G))))             ; return: (+ F G)

(defun make-subtraction (F G)
  (cond ((equal 0 F) (make-negative G))
	((equal 0 G) F)
	((and (numberp F) (numberp G)) (- F G))
	(t (list negative-symbol F G))))

(defun make-product (F G)
  (cond ((eq 0 F) 0)                            ; F*0=0
	((eq 0 G) 0)                            ; G*0=0
	((and (numberp F) (numberp G)) (* F G)) ; if they're numbers, multiply them
	(t (list product-symbol F G))))         ; return: (* F G)

(defun make-quotient(F G)
  (cond ((eq 0 F) 0)
	((eq 0 G) nil)
	((and (numberp F) (numberp G)) (/ F G))
	(t (list quotient-symbol F G))))

;;; Function make-power
;;  goal is to turn expt x 2 into (EXPT X 2)
(defun make-power(F G)
  (cond ((eq 0 F) 0)
	((eq 0 G) 1)
	((and (numberp F) (numberp G)) (expt F G))
	(t (list exponent-symbol F G))))


;;; Function make-negative
;;  goal is to turn (- x) into (- (x)) and (- 5) into -5
;;  also to turn (- - x) into (-(-x))
(defun make-negative(F)
  (cond ((numberp F) (list negative-symbol F))
	((variable-p F) (list negative-symbol F))
        ((negative-p F) (negative-operand F))
        (t (list negative-symbol F))))

;;; Function make-log
;;  goal is to fix 
(defun make-log(V)
  (list 'log V))

;;;==========================================================================
;;;INTEGRATION: CORE
;;;==========================================================================
(defun integrate (F V &optional lo hi)
  "Computes the integral of with respect to V"
  (def-integral (indef-integral F V) lo hi))
  ;(indef-integral F V))

(defun indef-integral (F V)
  "Computes the indefinite integral"
  (cond
    ((numberp F) (make-product F V))                                        ;; Handle integers 
    ((variable-p F) (make-product 1/2 (make-power F 2)))                    ;; Handle X
    ((sum-p F) (make-sum (indef-integral (sum-operand-1 F) V)               ;; Handle f(x) + g(x)
			 (indef-integral (sum-operand-2 F) V)))
    ((subtraction-p F) (make-subtraction                                    ;; Handle f(x) - g(x)
			(indef-integral (subtraction-operand-1 F) V)
			(indef-integral (subtraction-operand-2 F) V)))                            
    ((negative-p F)(make-negative (indef-integral (negative-operand F) V))) ;; Handle Negate-p 
    ((power-p F)(cond                                                       ;; Handle Power-p
       ((equal (power-operand-2 F) -1) (make-log V)) ;;Handle expt x -1
       (t (make-product (make-quotient 1 (1+ (power-operand-2 F)))
			(make-power (power-operand-1 F) (1+ (power-operand-2 F)))))))
    ))

	
	
 
(defun def-integral (F lo hi)
  "Computes the definite integral of F with respect to lo and hi values"
  (cond
    ((and lo hi) (- (eval (my-replace F hi)) (eval (my-replace F lo)))) ;(my-replace F lo))        ;; no values supplied for lo and hi
    (t F)))                      ;; return the indefinite integral

                            ;; else return the definite 
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


(defun my-replace (L val)
  "returns the list L with all occurrences of element e1 replaced bythe element e2"
  (cond
    ((endp L) nil)
    ((variable-p (first L)) (cons val (my-replace (rest L) val)))
    ;; For nested level
    ((listp (first L))   (cons (my-replace (first L) val) (my-replace (rest L) val)))
    (t                   (cons (first L) (my-replace (rest L) val)))
    ))



