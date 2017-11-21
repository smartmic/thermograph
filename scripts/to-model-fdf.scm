;;; to-model-fdf.scm
;;; Template for C code of wrapper for system of equations and
;;; Jacobian.
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
