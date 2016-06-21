;;; Macro for valve component
(define-syntax valve
  (syntax-rules ()
    ((_ media upstrm downstrm prm)
       (begin
           (populate-x (list upstrm downstrm))

           ;; Append component equations to global set of equations
           (set! eqns (append eqns (list 
            
           ; (i) mass balance
           (string-append
             (X_i upstrm "m") "-" (X_i downstrm "m"))
           ; (ii) isenthalpic process
           (string-append
             (X_i upstrm "h") "-" (X_i downstrm "h"))
           ; (iii) pressure relation (relative dp by parameter)
           (string-append
             (X_i upstrm "p") "*" (number->string prm) "-"
               (X_i downstrm "p"))))) 


           ;; Append to Jacobian: 
           (set! J (append J (list 

           ; (i) mass balance
           (string-append
               (if (integer? (car (slice upstrm))) 
                   (string-append 
                     "{{df}}["(number->string (car (slice upstrm)))"]=+1;")
                   "")
               (if (integer? (car (slice downstrm))) 
                   (string-append
                   "{{df}}["(number->string (car (slice downstrm)))"]=-1;")
                   ""))
           ; (ii) isenthalpic process
           (string-append
               (if (integer? (caddr (slice upstrm))) 
                   (string-append 
                     "{{df}}["(number->string (caddr (slice upstrm)))"]=+1;")
                   "")
               (if (integer? (caddr (slice downstrm))) 
                   (string-append
                   "{{df}}["(number->string (caddr (slice downstrm)))"]=-1;")
                   ""))
           ; (iii) pressure relation (relative dp by parameter)
           (string-append
               (if (integer? (cadr (slice upstrm))) 
                   (string-append
                   "{{df}}["(number->string (cadr (slice upstrm)))"]="
                     (number->string prm)";")
                   "")
               (if (integer? (cadr (slice downstrm))) 
                   (string-append
                   "{{df}}["(number->string (cadr (slice downstrm)))"]=-1;")
                   "")) ))) ))))


