;;; Establish the default package
;;; for symbols read in this file.
(defpackage new-match)

;;; Shadow any symbols from automatically inherited
;;; packages thathavethe same names as symbols
;;; in this package.
;;; (:shadow '(s1 s2))

;;; Shadow and import any symbols from other packages
;;; that are to be available as internal symbols
;;; in this package, but whose names conflict with
;;; symbols from automatically inherited packages.
;;; (:shadowing-import-from '(sym1 sym2))

;;; Specify any packages all of whose external
;;; symbols are to be accessible
;;; as internal symbols in this package.
;;; (:use '(package1 package2)

;;; Explicitly import any other symbols that are to
;;; be accessible as internal symbols
;;; in this package
;;; (:import-from '(symbol1 symbol2))

;;; Export the symbols from this package that are to
;;; be accessible to other packages.
;;; (:export '(symbol1 symbol2))

(in-package new-match)

(defun variablep (s)
  "takes one symbol as an argument and returns T if the first character's
name is #\? and returns NIL otherwise."
  (cond ((symbolp s) (eql (first (symbol-name s)) '#\?))
	(t NIL)))
