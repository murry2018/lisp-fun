(defparameter *prompt* ">>> ")
(loop named REPL do
     (format *query-io* *prompt*)
     (force-output *query-io*)
     (let ((input (read *query-io*)))
       (when (and (consp input)
                  (eql (car input) 'exit))
         (return-from REPL (cadr input)))
       (format t ";=> ~s~%" (eval input))))

#| result
>>> (print 'hi)

HI ;=> HI
>>> (+ 1 2)
;=> 3
>>> (exit 'bye)

'BYE
|#
