(defun deep-reverse(L)
  "This function performs a deep reverse, recursively"
  (cond(( endp L)          nil)
       ;;;Check if the next element is a list, if so, create a deep-reversed list of the list
       ((listp (first L)) (append (deep-reverse (rest L)) (list (deep-reverse (first L)))))
       (t                 (append (deep-reverse (rest L)) (list (first L))))))
