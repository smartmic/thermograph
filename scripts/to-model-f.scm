;;; to-model-f.scm
;;; Template for C code of system of equations.
;;; 
;;; This file is part of Thermograph.
;;; 
;;; Thermograph is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;; 
;;; Thermograph is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with Thermograph. If not, see <http://www.gnu.org/licenses/>.
;;; 
;;; Copyright 2016 Martin Michel

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

