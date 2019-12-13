(defun row-format (fact-digit prod-digit)
  (let ((fact-spec (write-to-string fact-digit))
        (prod-spec (write-to-string prod-digit)))
    (concatenate 'string
                 "|~{ ~" fact-spec "dx~" fact-spec
                 "d=~" prod-spec  "d |~}~%")))

(defun mult-table-row (min-level max-level column mult
                       fact-digit prod-digit)
  (format t (row-format fact-digit prod-digit)
          (loop repeat column
             for level from min-level to max-level
             append `(,level ,mult ,(* level mult)))))

(defun calc-digit (n)
  (1+ (truncate (/ (log n) (log 10)))))

(defun square (n) 
  (* n n))

(defun mult-table (&key (max-level 9) (column 3))
  (let ((fact-digit (calc-digit max-level))
        (prod-digit (calc-digit (square max-level))))
    (loop for level from 1 to max-level by column do
         (loop for mult from 1 to max-level do
              (mult-table-row level max-level column
                              mult fact-digit prod-digit))
         (princ #\Newline))))

;;; Example 1:
;; CL-USER> (mult-table :column 5)
;; | 1x1= 1 | 2x1= 2 | 3x1= 3 | 4x1= 4 | 5x1= 5 |
;; | 1x2= 2 | 2x2= 4 | 3x2= 6 | 4x2= 8 | 5x2=10 |
;; | 1x3= 3 | 2x3= 6 | 3x3= 9 | 4x3=12 | 5x3=15 |
;; | 1x4= 4 | 2x4= 8 | 3x4=12 | 4x4=16 | 5x4=20 |
;; | 1x5= 5 | 2x5=10 | 3x5=15 | 4x5=20 | 5x5=25 |
;; | 1x6= 6 | 2x6=12 | 3x6=18 | 4x6=24 | 5x6=30 |
;; | 1x7= 7 | 2x7=14 | 3x7=21 | 4x7=28 | 5x7=35 |
;; | 1x8= 8 | 2x8=16 | 3x8=24 | 4x8=32 | 5x8=40 |
;; | 1x9= 9 | 2x9=18 | 3x9=27 | 4x9=36 | 5x9=45 |
;;
;; | 6x1= 6 | 7x1= 7 | 8x1= 8 | 9x1= 9 |
;; | 6x2=12 | 7x2=14 | 8x2=16 | 9x2=18 |
;; | 6x3=18 | 7x3=21 | 8x3=24 | 9x3=27 |
;; | 6x4=24 | 7x4=28 | 8x4=32 | 9x4=36 |
;; | 6x5=30 | 7x5=35 | 8x5=40 | 9x5=45 |
;; | 6x6=36 | 7x6=42 | 8x6=48 | 9x6=54 |
;; | 6x7=42 | 7x7=49 | 8x7=56 | 9x7=63 |
;; | 6x8=48 | 7x8=56 | 8x8=64 | 9x8=72 |
;; | 6x9=54 | 7x9=63 | 8x9=72 | 9x9=81 |
;;
;;; Example 2:
;; CL-USER> (mult-table :max-level 13 :column 5)
;; |  1x 1=  1 |  2x 1=  2 |  3x 1=  3 |  4x 1=  4 |  5x 1=  5 |
;; |  1x 2=  2 |  2x 2=  4 |  3x 2=  6 |  4x 2=  8 |  5x 2= 10 |
;; |  1x 3=  3 |  2x 3=  6 |  3x 3=  9 |  4x 3= 12 |  5x 3= 15 |
;; |  1x 4=  4 |  2x 4=  8 |  3x 4= 12 |  4x 4= 16 |  5x 4= 20 |
;; |  1x 5=  5 |  2x 5= 10 |  3x 5= 15 |  4x 5= 20 |  5x 5= 25 |
;; |  1x 6=  6 |  2x 6= 12 |  3x 6= 18 |  4x 6= 24 |  5x 6= 30 |
;; |  1x 7=  7 |  2x 7= 14 |  3x 7= 21 |  4x 7= 28 |  5x 7= 35 |
;; |  1x 8=  8 |  2x 8= 16 |  3x 8= 24 |  4x 8= 32 |  5x 8= 40 |
;; |  1x 9=  9 |  2x 9= 18 |  3x 9= 27 |  4x 9= 36 |  5x 9= 45 |
;; |  1x10= 10 |  2x10= 20 |  3x10= 30 |  4x10= 40 |  5x10= 50 |
;; |  1x11= 11 |  2x11= 22 |  3x11= 33 |  4x11= 44 |  5x11= 55 |
;; |  1x12= 12 |  2x12= 24 |  3x12= 36 |  4x12= 48 |  5x12= 60 |
;; |  1x13= 13 |  2x13= 26 |  3x13= 39 |  4x13= 52 |  5x13= 65 |
;;
;; |  6x 1=  6 |  7x 1=  7 |  8x 1=  8 |  9x 1=  9 | 10x 1= 10 |
;; |  6x 2= 12 |  7x 2= 14 |  8x 2= 16 |  9x 2= 18 | 10x 2= 20 |
;; |  6x 3= 18 |  7x 3= 21 |  8x 3= 24 |  9x 3= 27 | 10x 3= 30 |
;; |  6x 4= 24 |  7x 4= 28 |  8x 4= 32 |  9x 4= 36 | 10x 4= 40 |
;; |  6x 5= 30 |  7x 5= 35 |  8x 5= 40 |  9x 5= 45 | 10x 5= 50 |
;; |  6x 6= 36 |  7x 6= 42 |  8x 6= 48 |  9x 6= 54 | 10x 6= 60 |
;; |  6x 7= 42 |  7x 7= 49 |  8x 7= 56 |  9x 7= 63 | 10x 7= 70 |
;; |  6x 8= 48 |  7x 8= 56 |  8x 8= 64 |  9x 8= 72 | 10x 8= 80 |
;; |  6x 9= 54 |  7x 9= 63 |  8x 9= 72 |  9x 9= 81 | 10x 9= 90 |
;; |  6x10= 60 |  7x10= 70 |  8x10= 80 |  9x10= 90 | 10x10=100 |
;; |  6x11= 66 |  7x11= 77 |  8x11= 88 |  9x11= 99 | 10x11=110 |
;; |  6x12= 72 |  7x12= 84 |  8x12= 96 |  9x12=108 | 10x12=120 |
;; |  6x13= 78 |  7x13= 91 |  8x13=104 |  9x13=117 | 10x13=130 |
;;
;; | 11x 1= 11 | 12x 1= 12 | 13x 1= 13 |
;; | 11x 2= 22 | 12x 2= 24 | 13x 2= 26 |
;; | 11x 3= 33 | 12x 3= 36 | 13x 3= 39 |
;; | 11x 4= 44 | 12x 4= 48 | 13x 4= 52 |
;; | 11x 5= 55 | 12x 5= 60 | 13x 5= 65 |
;; | 11x 6= 66 | 12x 6= 72 | 13x 6= 78 |
;; | 11x 7= 77 | 12x 7= 84 | 13x 7= 91 |
;; | 11x 8= 88 | 12x 8= 96 | 13x 8=104 |
;; | 11x 9= 99 | 12x 9=108 | 13x 9=117 |
;; | 11x10=110 | 12x10=120 | 13x10=130 |
;; | 11x11=121 | 12x11=132 | 13x11=143 |
;; | 11x12=132 | 12x12=144 | 13x12=156 |
;; | 11x13=143 | 12x13=156 | 13x13=169 |

