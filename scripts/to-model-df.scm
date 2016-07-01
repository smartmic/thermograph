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

