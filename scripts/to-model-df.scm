;;; to-model-df.scm
;;; Template for C code of Jacobian.
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

(let* ([filename "src/model_df.c"]
       [p (open-output-file filename)])
    (display (string-append "
#include <string.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_matrix.h>
#include <thermograph.h>

int model_df (const gsl_vector *x, void *params, gsl_matrix *J)
{
    double m[NN];
    double p[NN];
    double t[NN];
    memcpy(m,((struct rparams *) params)->m, sizeof(m)); 
    memcpy(p,((struct rparams *) params)->p, sizeof(p)); 
    memcpy(t,((struct rparams *) params)->t, sizeof(t)); 

    int i,j;
    double X[NE];
    double df[NE][NE] = {{0}};

    for (i=0; i<NE; i++) {
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

    for (i=0; i<NE; i++)
        for (j=0; j<NE; j++)
            gsl_matrix_set (J, i, j, df[i][j]);
    
    return GSL_SUCCESS;
}
") p) (close-port p))

