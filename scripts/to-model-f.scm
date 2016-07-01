;;; Outputs system of eqns
(let* ([filename "src/model_f.c"]
       [p (open-output-file filename)])
    (display (string-append "
#include <string.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_matrix.h>
#include <thermograph.h>

int model_f (const gsl_vector * x, void * params, gsl_vector * f)
{
    double m[NN];
    double p[NN];
    double t[NN];
    memcpy(m,((struct rparams *) params)->m, sizeof(m)); 
    memcpy(p,((struct rparams *) params)->p, sizeof(p)); 
    memcpy(t,((struct rparams *) params)->t, sizeof(t)); 

    int i;
    double X[NE];
    double Y[NE];

    for (i=0; i<NE; i++) {
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

    for (i=0; i<NE; i++) {
    gsl_vector_set (f, i, Y[i]);
    }
    
    return GSL_SUCCESS;
}") p) (close-port p))

