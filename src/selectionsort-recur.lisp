(defun selectionsort (v <)
  (selectsort-iter v 0 <))
(defun selectsort-iter (v i <)
  (if (= i (length v)) v
      (progn
        (swap v i (select v i < i))
        (selectsort-iter v (1+ i) <))))
(defun select (v i < g)
  (if (= i (length v)) g
      (if (funcall < (svref v i) (svref v g))
          (select v (1+ i) < i)
          (select v (1+ i) < g))))
(defun swap (v i j)
  (rotatef (svref v i) (svref v j)))

;;; Example:
;; CL-USER> (selectionsort #(1 7 9 4 0 8 2 1) #'<)
;; #(0 1 1 2 4 7 8 9)
;; CL-USER> (selectionsort #(1 7 9 4 0 8 2 1) #'>)
;; #(9 8 7 4 2 1 1 0)

