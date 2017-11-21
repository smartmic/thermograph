;;; to-thermograph.scm
;;; Template for C code of Thermograph's main code.
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

(let* ([filename "src/thermograph.c"]
       [p (open-output-file filename)])
  (display (string-append "
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_multiroots.h>

#include <thermograph.h>

int print_state (size_t iter, gsl_multiroot_fdfsolver * s)
{
  double pressure = gsl_vector_get (s->x,"(number->string (cadr (slice 4)))");
  double enthalpy = gsl_vector_get (s->x,"(number->string (caddr (slice 4)))");

  printf (\"iter = %3u T = %.3f\\n\",
          iter,
          T_ph(&pressure,&enthalpy));

  return 0;
}

int main (void)
{
  const gsl_multiroot_fdfsolver_type *T;
  gsl_multiroot_fdfsolver *s;

  int status;
  size_t i, iter = 0;

  const size_t n = NE;
  struct rparams p = {
      {274.0, 0.0, 28.5, 0.0, 3.0}, 
      {30.0, 0.0, 0.0, 0.0, 0.0},
      {40.5, 0.0, 70.7, 0.0, 0.0}};

  double x_init[NE];

  gsl_multiroot_function_fdf f = {&model_f, 
                              &model_df,
                              &model_fdf,
                              n, &p};


  gsl_vector *x = gsl_vector_alloc (n);

  for (i=0;i<NE;i++) {
      x_init[i] = 100;
      gsl_vector_set (x, i, x_init[i]);
  }

  T = gsl_multiroot_fdfsolver_newton;
  s = gsl_multiroot_fdfsolver_alloc (T, n);
  gsl_multiroot_fdfsolver_set (s, &f, x);

  print_state (iter, s);

  do
    {
      iter++;
      status = gsl_multiroot_fdfsolver_iterate (s);

      print_state (iter, s);

      if (status)   /* check if solver is stuck */
        break;

      status = 
        gsl_multiroot_test_residual (s->f, 1e-6);
    }
  while (status == GSL_CONTINUE && iter < 1000);

  printf (\"status = %s\\n\", gsl_strerror (status));

  gsl_multiroot_fdfsolver_free (s);
  gsl_vector_free (x);
  return 0;
} 
") p) (close-port p))
