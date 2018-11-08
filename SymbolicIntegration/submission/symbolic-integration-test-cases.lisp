;;; Name:        Timothy Wilson
;;; Class:       CSC 345
;;; Assignment:  Project 1
;;; Description: To contain the test data provided by Dr. Wyatt.

;;;==========================================================================

(defun t1()  (integrate '1 'x))

(defun t2()  (integrate '1 'y 1 4))

(defun t3()  (integrate 'z 'z))

(defun t4()  (integrate '(+ x 0) 'x))

(defun t5()  (integrate '(- x) 'x 1 3))

(defun t6()  (integrate '(- - x) 'x 1 4))

(defun t7()  (integrate '(- x) 'x))

(defun t8()  (integrate '(- - x) 'x))

(defun t9()  (integrate '(- - - x) 'x))

(defun t10() (integrate '(+ x (- x)) 'x))

(defun t11() (integrate '(- (+ (- - x) x)) 'x 1 4))

(defun t12() (integrate '(+ (+ (- - x) (+ x 3)) 2) 'x 2 6))

(defun t13() (integrate '(- x (expt x 3)) 'x))

(defun t14() (integrate '(- x (expt x 3)) 'x 2 5))

(defun t15() (integrate '(+ (+ x (- - - x)) (expt x 3)) 'x))

(defun t16() (integrate '(+ (- x (- x)) (expt x 3)) 'x 2 3))

(defun t17() (integrate '(expt x -1) 'x))

(defun t18() (integrate '(expt x -1) 'x 3 45))

(defun t19() (integrate '(+ (+ x (- 5 x)) (expt x -1)) 'x))

(defun t20() (integrate '(+ (+ x (- 5 x)) (expt x -1)) 'x 5 217))
