(defun gauss-elimination (m)
  (loop with (nrow ncol) = (array-dimensions m)
     with lead = 0  ; leading-1s made so far
     for c below (min nrow ncol)
     as sr =        ; find a row having nonzero column(sr: source row)
       (loop for r from lead below nrow 
          if (not (zerop (aref m r c))) return r)
     if (not (null sr)) do
       (loop with leading-item = (aref m sr c) ; make leading1 & row swap
          for i from c below ncol
          as tmp = (/ (aref m sr i) leading-item) do
            (setf (aref m sr i) (aref m lead i)
                  (aref m lead i) tmp))
       (loop for i from (+ lead 1) below nrow do
            (loop with current-leading = (aref m i c)
               for j from c below ncol
               as addition = (- (* current-leading (aref m lead j)))
               do (setf (aref m i j) (+ addition (aref m i j)))))
       (incf lead))
  m)

;;; Example 1:
;; CL-USER> (gauss-elimination #2a((1 2 1 0) (2 1 0 1)))
;; #2A((1 2 1 0) (0 1 2/3 -1/3))
;;
;;; Example 2:
;; CL-USER> (gauss-elimination #2a((5 3 2 -2)
;;                                 (1 1 0 2)
;;                                 (2 -1 1 3)))
;; #2A((1 3/5 2/5 -2/5) (0 1 -1 6) (0 0 1 -17/2))
