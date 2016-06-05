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

;;; setup and populate association list for mapping user node id to 
;;; X array index
(define populate-x
  (lambda (lis)
    (for-each (lambda (x) 
                (if (not (assoc x xmap))
                (set! xmap( assoc-set! xmap x (list (* 3 (length xmap)) #f)))))
              lis)))

;;; get internal key (index) from user supplied node id
(define x-get
  (lambda (key)
    (car (assoc-ref xmap key))))

;;; get numbered X node
(define X_i
  (lambda (node param)
    (let ((i 0))
      (cond 
        [(string=? param "m") (set! i 0)]
        [(string=? param "p") (set! i 1)]
        [(string=? param "h") (set! i 2)])
        (string-append "X[" (number->string (+ i node)) "]"))))

;;; get string for parameter node
(define paramptr
  (lambda (x node)
      (string-append x "[" (number->string node) "]")))

;;; Extract equation for boundary conditions
(define extract
  (lambda (blist) 
    (let ((node (car blist))(e ""))
      (set! eqns (append eqns 
        (map (lambda (x) (set! x (symbol->string x))
          (cond
            [(string=? x "m") 
                (string-append 
                  (X_i (x-get node) "m") "-" (paramptr x (/ (x-get node) 3)))]
            [(string=? x "p") 
                (string-append
                  (X_i (x-get node) "p") "-" (paramptr x (/ (x-get node) 3)))]
            [(string=? x "t") 
                (string-append 
                  (X_i (x-get node) "h") "-"
                  "h_pT(&"  (X_i (x-get node) "p") ",&"
                  (paramptr x (/ (x-get node) 3)) ")")]))
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
             (X_i (x-get upstrm) "m") "-" (X_i (x-get downstrm) "m"))
           ; isenthalpic process
           (string-append
             (X_i (x-get upstrm) "h") "-" (X_i (x-get downstrm) "h"))
           ; pressure relation (relative dp by parameter)
           (string-append
             (X_i (x-get upstrm) "p") "*" (number->string prm) "-"
               (X_i (x-get downstrm) "p")))))))))

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
             (map (lambda (x) 
                    (string-append "+" (X_i x "m"))) (map x-get upstrm)) 
             (map (lambda (x) 
                    (string-append "-" (X_i x "m"))) (map x-get downstrm)))))

             ; pressure identity (part 1): pressure of all inflow
             ; states equals the pressure of the first outflow state
             (map (lambda (x) 
               (string-append
                   "+" (X_i x "p") "-" (X_i (x-get (car downstrm)) "p")))
             (map x-get upstrm))

             ; pressure identity (part 2): the pressure of all outflow
             ; states is equal
             (map (lambda (x) 
               (string-append
                   "+" (X_i (x-get (car downstrm)) "p") "-" (X_i x "p")))
             (map x-get (cdr downstrm)))

             ; enthalpy identity: the enthalpy of all outflow states
             ; is equal
             (map (lambda (x) 
               (string-append
                   "+" (X_i (x-get (car downstrm)) "h") "-" (X_i x "h")))
             (map x-get (cdr downstrm)))

             ; energy balance
             (list (string-concatenate (append
               (map (lambda (x) 
                   (string-append "+" (X_i x "m") "*" (X_i x "h")))
                     (map x-get upstrm)) 
               (map (lambda (x) 
                   (string-append "-" (X_i x "m") "*" (X_i x "h")))
                     (map x-get downstrm))))) )))))))

;;; Macro for injecting boundary conditions into system
(define-syntax provide-params
  (syntax-rules ()
        ((_ (node var* ...)* ...)
         (begin
           (for-each extract (list (node var* ...)* ... ))))))

;;; Customized model definition 
(valve ws (in 1) (out 2) (param 0.99)) 
(header (in '(2 3)) (out '(4 5)))
(provide-params 
   '(1 p m t)
   '(3 m t)
   '(5 m))
   

;;; Outputs
(display (string-append "
double h_pT(double *, double *);

struct rparams
{
    double m["(number->string (length xmap))"];
    double p["(number->string (length xmap))"];
    double t["(number->string (length xmap))"];
};

int model_f (const gsl_vector * x, void * params, gsl_vector * f)
{
    double m["(number->string (length xmap))"];
    double p["(number->string (length xmap))"];
    double t["(number->string (length xmap))"];
    memcpy(m,((struct rparams *) params)->m, sizeof(m)); 
    memcpy(p,((struct rparams *) params)->p, sizeof(p)); 
    memcpy(t,((struct rparams *) params)->t, sizeof(t)); 

    int i;
    double X["(number->string (length eqns))"];
    double Y["(number->string (length eqns))"];

    for (i=0; i<"(number->string (length eqns))"; i++) {
    X[i] = gsl_vector_get (x, i);
    }

    // START OF EQNS"))

(for-each
  (lambda (lhs rhs) (display (string-append lhs rhs ";")))
    (map 
      (lambda (x) 
        (string-append "\n    Y[" (number->string x) "]=")) (iota (length eqns)))
    eqns)

(display (string-append "
    // END OF EQNS

    for (i=0; i<"(number->string (length eqns))"; i++) {
    gsl_vector_set (f, i, Y[i]);
    }
    
    return GSL_SUCCESS;
}"))
