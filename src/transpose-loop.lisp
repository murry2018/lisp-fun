(defun slice (m)
  (loop for (head . tail) in m
        for endp = (null tail)
            then (or endp (null tail))
     collect head into heads
     collect tail into tails
     finally (return `(,heads ,tails . ,endp))))
(defun transpose (m)
  (loop for (heads tails . endp) = (slice m)
            then (slice tails)
     collect heads into mt
     if endp return mt))

;;; example:
;; CL-USER> (defparameter *matrix*
;;   '((a11 a12 a13 a14 a15)
;;     (a21 a22 a23 a24 a25)
;;     (a31 a32 a33 a34 a35)
;;     (a41 a42 a43 a44 a45)))
;; CL-USER> (transpose *matrix*)
;; ((A11 A21 A31 A41) 
;;  (A12 A22 A32 A42) 
;;  (A13 A23 A33 A43)
;;  (A14 A24 A34 A44)
;;  (A15 A25 A35 A45))

