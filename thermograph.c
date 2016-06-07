#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_multiroots.h>

#include <model_f.h>
#include <model_df.h>
#include <ws_table.h>

int model_f (const gsl_vector *, void *, gsl_vector *);
int model_df (const gsl_vector *, void *, gsl_matrix *);

double T_ph(double *, double *);

int print_state (size_t iter, gsl_multiroot_fdfsolver * s)
{
  double pressure = gsl_vector_get (s->x,13);
  double enthalpy = gsl_vector_get (s->x,14);

  printf ("iter = %3u x = % .3f % .3f % .3f % .3f "
          "T4 = %.3f\n",
          iter,
          gsl_vector_get (s->x, 0), 
          gsl_vector_get (s->x, 1),
          gsl_vector_get (s->x, 10),
          gsl_vector_get (s->x, 11),
          T_ph(&pressure,&enthalpy));

  return 0;
}

int main (void)
{
  const gsl_multiroot_fdfsolver_type *T;
  gsl_multiroot_fdfsolver *s;

  int status;
  size_t i, iter = 0;

  const size_t n = 15;
  struct rparams p = {
      {274.0, 0.0, 28.5, 0.0, 3.0}, 
      {30.0, 0.0, 0.0, 0.0, 0.0},
      {40.5, 0.0, 70.7, 0.0, 0.0}};
  gsl_multiroot_function_fdf f = {&model_f, 
                              &model_df,
                              &model_fdf,
                              n, &p};

  double x_init[15] = {100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,};

  gsl_vector *x = gsl_vector_alloc (n);

  for (i=0;i<15;i++) {
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

  printf ("status = %s\n", gsl_strerror (status));

  gsl_multiroot_fdfsolver_free (s);
  gsl_vector_free (x);
  return 0;
} 

