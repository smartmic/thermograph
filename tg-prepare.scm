#!/usr/bin/guile -s 
!#
;;;coding:utf-8

;;; Use UTF-8 
(setlocale LC_ALL "")
(use-modules (ice-9 regex))
(use-modules (srfi srfi-1))


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
(define J '())

;;; setup and populate association list for mapping user node id to 
;;; X array index
(define populate-x
  (lambda (lis)
    (for-each (lambda (x) 
                (if (not (assoc x xmap))
                (set! xmap (assoc-set! xmap x 
                (cons (* 3 (length xmap)) (iota 3 (* 3 (length xmap)) 1))))))
              lis)))

;;; get internal key (index) from user supplied node id
(define x-get
  (lambda (key)
    (car (assoc-ref xmap key))))

;;; Extraction of slice (m p h) for given node from xmap
(define slice
  (lambda (key)
    (cdr (assq-ref xmap key))))

;;; get numbered X node
(define X_i
  (lambda (node param)
      (let ([entry #f])
        (cond 
        [(string=? param "m") (set! entry (car (slice node)))]
        [(string=? param "p") (set! entry (cadr (slice node)))]
        [(string=? param "h") (set! entry (caddr (slice node)))])
      (if (integer? entry) 
      (string-append "X[" (number->string entry) "]") 
      entry) )))

(define bcs/cc #f)

;(define model
;  (lambda (name)
;    (call/cc (lambda (k) (set! bcs/cc k)))))
;
(include "scripts/bcs-inject.scm")
;(include "scripts/bcs.scm")
(include "scripts/valve-inject.scm")
(include "scripts/header-inject.scm")

(include "def.scm") 

; reset eqns and call continuations
;(set! eqns '())
;(set! J '())
;(set! xmap '())
;(bcs/cc)

(newline)
(write eqns)
(newline)
(newline)
(write xmap)
(newline)

;;; Outputs system of eqns
(let* ([filename "model_fdf.h"]
       [p (open-output-file filename)])
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

    // START OF EQNS") p)

    (for-each
      (lambda (lhs rhs) (display (string-append lhs rhs ";") p))
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
}") p)

    ;;; Outputs Jacobian
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

    // START OF Jacobian entries\n\n") p)

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
                        "\n") p))
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
}

int model_fdf (const gsl_vector * x, void *params, gsl_vector * f, gsl_matrix * J)
{
    model_f (x, params, f);
    model_df (x, params, J);
    
    return GSL_SUCCESS;
}
") p)
(close-port p))
