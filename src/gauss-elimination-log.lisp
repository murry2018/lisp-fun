(defun print-mat (m)
  (loop with (nrow ncol) = (array-dimensions m)
     for r below nrow do
       (loop for c below ncol do
            (format t "~6a " (aref m r c)))
       (princ #\Newline)))

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
       (print-mat m)
       (princ #\Newline)
       (loop for i from (+ lead 1) below nrow do
            (loop with current-leading = (aref m i c)
               for j from c below ncol
               as addition = (- (* current-leading (aref m lead j)))
               do (setf (aref m i j) (+ addition (aref m i j)))))
       (print-mat m)
       (princ #\Newline)
       (incf lead))
  m)

;;; Example:
;; CL-USER> (gauss-elimination #2a((1 3 1 9)
;;                                 (1 1 -1 1)
;;                                 (3 11 5 35)))
;; 1      3      1      9      
;; 1      1      -1     1      
;; 3      11     5      35     

;; 1      3      1      9      
;; 0      -2     -2     -8     
;; 0      2      2      8      

;; 1      3      1      9      
;; 0      1      1      4      
;; 0      2      2      8      

;; 1      3      1      9      
;; 0      1      1      4      
;; 0      0      0      0      

;; #2A((1 3 1 9) (0 1 1 4) (0 0 0 0))
