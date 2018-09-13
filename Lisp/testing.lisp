(defun deep-reverse(L)
  "This function performs a deep reverse, recursively"
  (cond(( endp L)    nil)
       (( listp L) ( append( deep-reverse( rest L)) ( list ( first L)))))
       (t          ( append( deep-reverse( rest L)) ( list ( first L))))))
