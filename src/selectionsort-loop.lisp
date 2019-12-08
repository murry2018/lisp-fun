(defun swap (v i j)
  (let ((tmp (svref v i)))
    (setf (svref v i) (svref v j)
          (svref v j) tmp)))
(defun selectionsort (v <)
  (loop for i below (length v)
     as select =
       (loop for j from i below (length v) 
          with select = i
          if (funcall < (svref v j) (svref v select)) do
            (setf select j)
          finally (return select))
     do (swap v i select)
     finally (return v)))

#| example
CL-USER> (selectionsort #(1 7 9 4 0 8 2 1) #'<)
#(0 1 1 2 4 7 8 9)
CL-USER> (selectionsort #(1 7 9 4 0 8 2 1) #'>)
#(9 8 7 4 2 1 1 0)
|#