#!/usr/bin/guile -s
!#
;;;coding:utf-8

;;; Use UTF-8 
(setlocale LC_ALL "")

;;; util procedures
(define (reduce fn base-value lis)
  (if (null? lis)
    base-value
    (fn (car lis)
        (reduce fn base-value (cdr lis)))))

;;; various definitions
(define ws "water")
(define boundary-list '(0 2))
(define p "p")
(define m "m")
(define t "t")
(define in (lambda (n) n))
(define out (lambda (n) n))
(define param (lambda (n) n))
(define xmap '())
(define eqns '())
(define eqno -1)

;;; setup and populate association list for mapping user node id to C
;;; index
(define populate-x
  (lambda (lis)
    (for-each (lambda (x) 
                (if (not (assoc x xmap))
                  (set! xmap 
                     (assoc-set! xmap x (list (* 3 (length xmap)) #f)))))
              lis)))

;;; get internal key (index) from user supplied node id
(define x-get
  (lambda (key)
    (car (assoc-ref xmap key))))

;;; get numbered X node
(define x-io
  (lambda (node sign param)
    (let ((i 0))
      (cond 
        [(string=? param "m") (set! i 0)]
        [(string=? param "p") (set! i 1)]
        [(string=? param "h") (set! i 2)])
        (string-concatenate 
          (list (string sign) "X[" (number->string (+ i node)) "]")))))

;;; get numbered string for f(x) aka Y[i]
(define Y
  (lambda ()
    (set! eqno (1+ eqno))
      (string-concatenate (list 
       "\nY[" (number->string eqno) "] = "))))

;;; get string for parameter node
(define paramptr
  (lambda (x node)
      (string-concatenate (list
        "-" x "[" (number->string node) "]"))))

;;; Extract equation for boundary conditions
(define extract
  (lambda (blist) 
    (let ((node (car blist))(e ""))
      (set! eqns (append eqns 
        (map (lambda (x) (set! x (symbol->string x))
          (cond
            [(string=? x "m") 
                (string-append 
                  (Y) (x-io (x-get node) #\+ "m") (paramptr x (x-get node)))]
            [(string=? x "p") 
                (string-append
                  (Y) (x-io (x-get node) #\+ "p") (paramptr x (x-get node)))]
            [(string=? x "t") 
                (string-append 
                  (Y) (x-io (x-get node) #\+ "h") (paramptr x (x-get node)))]))
        (cdr blist)))))))

;;; Macro for valve component
(define-syntax valve
  (syntax-rules ()
    ((_ media upstrm downstrm prm)
       (begin
           (populate-x (list upstrm downstrm))

           ; append component equations to global set of equations
           (set! eqns (append eqns (list 
            
           ; mass balance
           (string-append
             (Y) (x-io (x-get upstrm) #\+ "m") (x-io (x-get downstrm) #\- "m"))
           ; isenthalpic process
           (string-append
             (Y) (x-io (x-get upstrm) #\+ "h") (x-io (x-get downstrm) #\- "h"))
           ; pressure relation (relative dp by parameter)
           (string-append
             (Y) (x-io (x-get upstrm) #\+ "p") "*" (number->string prm)
               (x-io (x-get downstrm) #\- "p")))))))))

;;; Macro for header component (multi-mixer-splitter)
(define-syntax header
  (syntax-rules ()
        ((_ upstrm downstrm)
         (begin
           (populate-x (append upstrm downstrm))

           ; append component equations to global set of equations
           (set! eqns (append eqns (append 
            ; form list of equations as one-line strings
            (list (string-concatenate (append

             ; mass balance
             (list (Y))
                   (map (lambda (x) (x-io x  #\+ "m")) (map x-get upstrm)) 
                   (map (lambda (x) (x-io x  #\- "m")) (map x-get downstrm)))))

             ; pressure identity (part 1): pressure of all inflow
             ; states equals the pressure of the first outflow state
             (map (lambda (x) 
               (string-append
                   (Y) (x-io x #\+ "p") (x-io (x-get (car downstrm)) #\- "p")))
             (map x-get upstrm))

             ; pressure identity (part 2): the pressure of all outflow
             ; states is equal
             (map (lambda (x) 
               (string-append
                   (Y) (x-io (x-get (car downstrm)) #\+ "p") (x-io x #\- "p")))
             (map x-get (cdr downstrm)))

             ; enthalpy identity: the enthalpy of all outflow states
             ; is equal
             (map (lambda (x) 
               (string-append
                   (Y) (x-io (x-get (car downstrm)) #\+ "h") (x-io x #\- "h")))
             (map x-get (cdr downstrm)))

             ; energy balance
             (list (string-concatenate (append (list (Y))
               (map (lambda (x) 
                   (string-append (x-io x  #\+ "m") "*" (x-io x  #\si  "h")))
                     (map x-get upstrm)) 
               (map (lambda (x) 
                   (string-append (x-io x  #\- "m") "*" (x-io x  #\si "h")))
                     (map x-get downstrm))))))))) )))

;;; Macro for injecting boundary conditions into system
(define-syntax provide-params
  (syntax-rules ()
        ((_ (node var* ...)* ...)
         (begin
           (for-each extract (list (node var* ...)* ... ))))))

;;; Customized model definition (using specific DSL)
(valve ws (in 1) (out 2) (param 0.99)) 
(header (in '(2 3)) (out '(4 5)))
(provide-params 
   '(1 p m t)
   '(3 m t)
   '(5 m))
   

;;; Runtime outputs
(display (length eqns))
(display eqns)
(newline)
(display xmap)
