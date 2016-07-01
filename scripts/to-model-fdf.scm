(let* ([filename "src/model_fdf.c"]
       [p (open-output-file filename)])
  (display (string-append "
#include <gsl/gsl_vector.h>
#include <gsl/gsl_matrix.h>
#include <thermograph.h>

int model_fdf (const gsl_vector *x, void *params, gsl_vector *f, gsl_matrix *J)
{
  model_f (x, params, f);
  model_df (x, params, J);

  return GSL_SUCCESS;
}
") p) (close-port p))
