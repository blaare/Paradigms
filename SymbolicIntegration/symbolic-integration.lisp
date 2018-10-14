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
(defun negative-operands (F)(second F))

;; In the case of + a b
;; 'a' corresponds to sum-operand-1
(defun sum-operand-1 (F)(second F))
;; 'b' corresponds to sum-operand-2
(defun sum-operand-2 (F)(third F))


;;;==========================================================================
;;; PREDICATES

(defun variable-p (F)
  (member F variable-symbols))

;;; Function negative-p
;;  returns t if F is -
(defun negative-p (F)
  (eq (first F) negative-symbol))

;;; Function sum-p
;;  returns t if the F is +
(defun sum-p (F)
  (eq (first F) sum-symbol))

;;; Function product-p
;;  returns t if the first element is *
(defun product-p (F)
  (eq (first F) product-symbol))

;;; Function quotient-p
;;  returns t if the first element is /
(defun quotient-p (F)
  (eq (first F) quotient-symbol))

;;;==========================================================================
;;; CONSTRUCTORS
(defun make-variable (V) V)
;;; NOTE that this function, whcih seems 'trivial', is included
;;; for the completeness of the abstraction from the way the math
;;; functions are represented in Lisp

(defun make-sum (F G)
  (cond ((eq 0 F) G)                            ; G+0=G
	((eq 0 G) F)                            ; F+0=F
	;; there might be other cases you need to consider
	((and (numberp F) (numberp G)) (+ F G)) ; if they're numbers, add them
	(t (list sum-symbol F G))))             ; return: (+ F G)

(defun make-product (F G)
  (cond ((eq 0 F) 0)                            ; F*0=0
	((eq 0 G) 0)                            ; G*0=0
	;; there might be other cases you need to consider
	((and (numberp F) (numberp G)) (* F G)) ; if they're numbers, multiply them
	(t (list product-symbol F G))))         ; return: (* F G)

(defun make-quotient(F G)
  (cond ((eq 0 F) 0)
	((eq 0 G) nil)
	;; there might be other cases you need to consider
	((and (numberp F) (numberp G)) (/ F G))
	(t (list quotient-symbol F G))))

;;; Function make-power
;;  goal is to turn expt x 2 into (EXPT X 2)
(defun make-power(F G)
  (cond ((eq 0 F) 0)
	((eq 0 G) 1)
	;; ther might be other cases you need to consider
	((and (numberp F) (numberp G)) (expt F G))
	(t (list exponent-symbol F G))))

;;; Function make-negative
;;  goal is to turn (- x) into (- (x)) and (- 5) into -5
(defun make-negative(F G)
  (cond ((eq 0 F) G)
	((eq 0 G) 0)
	;; there might be other cases you need to consider
	((numberp G) (eval (list negative-symbol G)))
	(t (list negative-symbol (list G)))))

;;;==========================================================================
;;;INTEGRATION: CORE
;;;==========================================================================
(defun integrate (F V &optional lo hi)
  "Computes the integral of with respect to V"
  (def-integral (indef-integral F V) lo hi))

(defun indef-integral (F V)
  "Computes the indefinite integral"
  (cond
    ((numberp F) (make-product F V))
    ;; Handle x
    ((variable-p F) (make-product 1/2 (make-power F 2)))
    ;; Handle f(x) + g(x)
    ((sum-p F) (make-sum
		(indef-integral (sum-operand-1 F) V)
		(indef-integral (sum-operand-2 F)V)))
    ;; Handle -x

    ((negative-p F) (list
		     negative-symbol
		     (indef-integral (negative-operands F) V)))
    ))

	
	

(defun def-integral (F lo hi)
  "Computes the definite integral of F with respect to lo and hi values"
    (if (and lo hi) F) ;; no values supplied for lo and hi
    F)                        ;; return the indefinite integral
                            ;; else return the definite integral

