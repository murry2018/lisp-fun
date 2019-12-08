(defun star-row (n)
  (loop repeat n do
       (princ #\*)))
(defun star-downtrape (s e)
  (loop for n from s downto e do
       (star-row n)
       (princ #\Newline)))
(defun star-uptrape (s e)
  (loop for n from s to e do
       (star-row n)
       (princ #\Newline)))
(defun star-trape (s e)
  (if (< s e) (star-uptrape s e)
      (star-downtrape s e)))

;;; example:
;; CL-USER> (progn (star-trape 1 4) (star-trape 3 1))
;; *
;; **
;; ***
;; ****
;; ***
;; **
;; *
;; CL-USER> (progn (star-trape 7 5) (star-trape 6 7))
;; *******
;; ******
;; *****
;; ******
;; *******
