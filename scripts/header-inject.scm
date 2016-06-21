;;; Macro for header component (multi-mixer-splitter)
(define-syntax header
  (syntax-rules ()
        ((_ upstrm downstrm)
         (begin
           (populate-x (append upstrm downstrm))
           (append-header-eqns! upstrm downstrm)
           (append-header-Jacobian! upstrm downstrm) ))))

;;; Procedure to update global variable 'eqns' with component related
;;; equations
(define append-header-eqns!
  (lambda (upstrm downstrm)
    (set! eqns 
      (append 
        eqns (append 
               ; form list of equations as one-line strings
               (list (string-concatenate 
                       (append
                         ; mass balance
                         (map (lambda (x) 
                                (string-append "+" (X_i x "m"))) upstrm) 
                         (map (lambda (x) 
                                (string-append "-" (X_i x "m"))) downstrm))))

               ; pressure identity (part 1): pressure of all inflow
               ; states equals the pressure of the first outflow state
               (map (lambda (x) 
                      (string-append
                        "+" (X_i x "p") "-" (X_i (car downstrm) "p")))
                    upstrm)

               ; pressure identity (part 2): the pressure of all outflow
               ; states is equal
               (map (lambda (x) 
                      (string-append
                        "+" (X_i (car downstrm) "p") "-" (X_i x "p")))
                    (cdr downstrm))

               ; enthalpy identity: the enthalpy of all outflow states
               ; is equal
               (map (lambda (x) 
                      (string-append
                        "+" (X_i (car downstrm) "h") "-" (X_i x "h")))
                    (cdr downstrm))

               ; energy balance
               (list (string-concatenate 
                       (append
                         (map (lambda (x) 
                                (string-append "+" (X_i x "m") "*" (X_i x "h")))
                              upstrm) 
                         (map (lambda (x) 
                                (string-append "-" (X_i x "m") "*" (X_i x "h")))
                              downstrm)))) ))))) 

;;; Procedure to update global variable 'J' (Jacobian) with partial
;;; derivatives from above provided equations
(define append-header-Jacobian! 
  (lambda (upstrm downstrm)
    (set! J 
      (append 
        J (append 
            ; dY/dX's from mass balance
            (list (string-concatenate 
                    (append
                      (map (lambda (x) 
                             (if (integer? (car (slice x))) 
                               (string-append 
                                 "{{df}}["(number->string (car (slice x)))"]=+1;")
                               ""))
                           upstrm) 
                      (map (lambda (x) 
                             (if (integer? (car (slice x))) 
                               (string-append 
                                 "{{df}}["(number->string (car (slice x)))"]=-1;")
                               ""))
                           downstrm))))

            ; pressure identity (part 1): pressure of all inflow
            ; states equals the pressure of the first outflow state
            ; dY/dX's 
            (map (lambda (x) 
                   (string-append
                     (if (integer? (cadr (slice x))) 
                       (string-append
                         "{{df}}["(number->string (cadr (slice x)))"]=+1;") 
                       "")
                     (if (integer? (cadr (slice (car downstrm))))
                       (string-append
                         "{{df}}["(number->string (cadr (slice (car downstrm))))"]=-1;")
                       "")))
                 upstrm)

            ; pressure identity (part 2): the pressure of all outflow
            ; states is equal
            ; dY/dX's 
            (map (lambda (x) 
                   (string-append
                     (if (integer? (cadr (slice (car downstrm))))
                       (string-append
                         "{{df}}["(number->string (cadr (slice (car downstrm))))"]=+1;")
                       "")
                     (if (integer? (cadr (slice x)))
                       (string-append
                         "{{df}}["(number->string (cadr (slice x)))"]=-1;")
                       "")))
                 (cdr downstrm))

            ; enthalpy identity: the enthalpy of all outflow states
            ; is equal
            ; dY/dX's 
            (map (lambda (x) 
                   (string-append
                     (if (integer? (caddr (slice (car downstrm))))
                       (string-append
                         "{{df}}["(number->string (caddr (slice (car downstrm))))"]=+1;")
                       "")
                     (if (integer? (caddr (slice x)))
                       (string-append
                         "{{df}}["(number->string (caddr (slice x)))"]=-1;")
                       "")))
                 (cdr downstrm))

            ; energy balance
            ; dY/dX's 
            (list (string-concatenate 
                    (append
                      (map (lambda (x) 
                             (string-append 
                               (if (integer? (car (slice x)))
                                 (string-append
                                   "{{df}}[" (number->string (car (slice x))) "]=+"
                                   (if (integer? (caddr (slice x)))
                                     (X_i x "h")
                                     (caddr (slice x))) ";" )
                                 "")
                               (if (integer? (caddr (slice x)))
                                 (string-append
                                   "{{df}}[" (number->string (caddr (slice x))) "]=+"
                                   (if (integer? (car (slice x)))
                                     (X_i x "m")
                                     (car (slice x))) ";" )
                                 "")))
                           upstrm) 
                      (map (lambda (x) 
                             (string-append 
                               (if (integer? (car (slice x)))
                                 (string-append
                                   "{{df}}[" (number->string (car (slice x))) "]=+" 
                                   (if (integer? (caddr (slice x)))
                                     (X_i x "h")
                                     (caddr (slice x))) ";" )
                                 "")
                               (if (integer? (caddr (slice x)))
                                 (string-append
                                   "{{df}}[" (number->string (caddr (slice x))) "]=-" 
                                   (if (integer? (car (slice x)))
                                     (X_i x "m")
                                     (car (slice x))) ";" )
                                 "")))
                           downstrm)))) ))))) 
