#!/usr/bin/guile -s 
!#
;;;coding:utf-8

;;; Use UTF-8 
(setlocale LC_ALL "")
(use-modules (ice-9 regex))

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
(define J '())

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
      (set! J (append J 
        (map (lambda (x) (set! x (symbol->string x))
          (cond
            [(string=? x "m") 
                (string-append 
                  "{{df}}[" (number->string (x-get node)) "]=+1;")]
            [(string=? x "p") 
                (string-append
                  "{{df}}[" (number->string (+ 1 (x-get node))) "]=+1;")]
            [(string=? x "t") 
                (string-append 
                  "{{df}}[" (number->string (+ 2 (x-get node))) "]=+1;")]))
        (cdr blist)))))))

;;; Macro for valve component
(define-syntax valve
  (syntax-rules ()
    ((_ media upstrm downstrm prm)
       (begin
           (populate-x (list upstrm downstrm))

           ; append component equations to global set of equations
           (set! J (append J (list 
            
           ; mass balance
           (string-append 
             "{{df}}["(number->string (x-get upstrm))"]=+1;"
             "{{df}}["(number->string (x-get downstrm))"]=-1;")
           ; isenthalpic process
           (string-append 
             "{{df}}["(number->string (+ 2 (x-get upstrm)))"]=+1;"
             "{{df}}["(number->string (+ 2 (x-get downstrm)))"]=-1;")
           ; pressure relation (relative dp by parameter)
           (string-append
             "{{df}}["(number->string (+ 1 (x-get upstrm)))"]="
             (number->string prm)";"
             "{{df}}["(number->string (+ 1 (x-get downstrm)))"]=-1;"))))))))

;;; Macro for header component (multi-mixer-splitter)
(define-syntax header
  (syntax-rules ()
        ((_ upstrm downstrm)
         (begin
           (populate-x (append upstrm downstrm))

           ; append component equations to global set of equations
           (set! J (append J (append 
            ; form list of equations as one-line strings
            (list (string-concatenate (append

             ; mass balance
             (map (lambda (x) 
                    (string-append 
                      "{{df}}["(number->string x)"]=+1;")) 
                  (map x-get upstrm)) 
             (map (lambda (x) 
                    (string-append 
                      "{{df}}["(number->string x)"]=-1;")) 
                  (map x-get downstrm)))))

             ; pressure identity (part 1): pressure of all inflow
             ; states equals the pressure of the first outflow state
             (map (lambda (x) 
               (string-append
                   "{{df}}["(number->string (+ 1 x))"]=+1;" 
                   "{{df}}["(number->string (+ 1 (x-get (car downstrm))))"]=-1;"))
             (map x-get upstrm))

             ; pressure identity (part 2): the pressure of all outflow
             ; states is equal
             (map (lambda (x) 
               (string-append
                   "{{df}}["(number->string (+ 1 (x-get (car downstrm))))"]=+1;"
                   "{{df}}["(number->string (+ 1 x))"]=-1;"))
             (map x-get (cdr downstrm)))

             ; enthalpy identity: the enthalpy of all outflow states
             ; is equal
             (map (lambda (x) 
               (string-append
                   "{{df}}["(number->string (+ 2 (x-get (car downstrm))))"]=+1;"
                   "{{df}}["(number->string (+ 2 x))"]=-1;"))
             (map x-get (cdr downstrm)))

             ; energy balance
             (list (string-concatenate (append
               (map (lambda (x) 
                   (string-append 
                     "{{df}}[" (number->string x) "]=+" (X_i x "h") ";"
                     "{{df}}[" (number->string (+ 2 x)) "]=+" (X_i x "m") ";" ))
                     (map x-get upstrm)) 
               (map (lambda (x) 
                   (string-append 
                     "{{df}}[" (number->string x) "]=+" (X_i x "h") ";"
                     "{{df}}[" (number->string (+ 2 x)) "]=-" (X_i x "m") ";"))
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
int model_df (const gsl_vector * x, void * params, gsl_matrix * J)
{
    double m["(number->string (length xmap))"];
    double p["(number->string (length xmap))"];
    double t["(number->string (length xmap))"];
    memcpy(m,((struct rparams *) params)->m, sizeof(m)); 
    memcpy(p,((struct rparams *) params)->p, sizeof(p)); 
    memcpy(t,((struct rparams *) params)->t, sizeof(t)); 

    int i,j;
    double X["(number->string (length J))"];
    double df["(number->string (length J))"]["(number->string (length J))"] = {{0}};

    for (i=0; i<"(number->string (length J))"; i++) {
    X[i] = gsl_vector_get (x, i);
    }

    // START OF Jacobian entries\n\n"))

;;; Print the matrix elements of Jacobian based on {{df}} template
;;; which will be replaced by function number and X definition from
;;; component library
(for-each
  ; applicable function: regex-substitute {{df}} with df[#]
  ; first input e: entries from components J
  ; second input r: numbered df(x) as list
  (lambda (e r) (display 
                  (string-append 
                    "    " 
                    (regexp-substitute/global #f "\\{\\{df\\}\\}" e 'pre r 'post) 
                    "\n")))
    J 
    (map 
      (lambda (x) 
       (string-append "df[" (number->string x) "]") )
      (iota (length J))) )

(display (string-append "
    // END OF Jacobian entries

    for (i=0; i<"(number->string (length J))"; i++)
        for (j=0; j<"(number->string (length J))"; j++)
            gsl_matrix_set (J, i, j, df[i][j]);
    
    return GSL_SUCCESS;
}"))
