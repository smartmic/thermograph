#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_multiroots.h>
#include <steam_pT.h>
#include <steam_ph.h>

#include <model_f.h>

int model_f (const gsl_vector *, void *, gsl_vector *);

int print_state (size_t iter, gsl_multiroot_fsolver * s)
{
  SteamState S = freesteam_set_ph(gsl_vector_get (s->x,7)*1e5, gsl_vector_get (s->x,11)*1e3);

  printf ("iter = %3u x = % .3f % .3f "
          "f(x) = % .3e % .3e "
          "T4 = %.3f\n",
          iter,
          gsl_vector_get (s->x, 0), 
          gsl_vector_get (s->x, 1),
          gsl_vector_get (s->f, 0), 
          gsl_vector_get (s->f, 1),
          freesteam_T(S)-273.15);

  return 0;
}

int main (void)
{
  const gsl_multiroot_fsolver_type *T;
  gsl_multiroot_fsolver *s;

  int status;
  size_t i, iter = 0;

  const size_t n = 12;
  struct rparams p = {
      {274.0, 0.0, 28.5, 0.0}, 
      {30.0, 0.0, 0.0, 0.0},
      {40.5, 0.0, 70.7, 0.0}};
  gsl_multiroot_function f = {&model_f, n, &p};

  double x_init[12] = {184.0, 184.0, 184.0, 184.0, 184.0, 184.0, 184.0, 184.0, 184.0, 184.0, 184.0, 184.0 };
  gsl_vector *x = gsl_vector_alloc (n);

  for (i=0;i<12;i++) {
      gsl_vector_set (x, i, x_init[i]);
  }

  T = gsl_multiroot_fsolver_hybrids;
  s = gsl_multiroot_fsolver_alloc (T, n);
  gsl_multiroot_fsolver_set (s, &f, x);

  print_state (iter, s);

  do
    {
      iter++;
      status = gsl_multiroot_fsolver_iterate (s);

      print_state (iter, s);

      if (status)   /* check if solver is stuck */
        break;

      status = 
        gsl_multiroot_test_residual (s->f, 1e-6);
    }
  while (status == GSL_CONTINUE && iter < 1000);

  printf ("status = %s\n", gsl_strerror (status));

  gsl_multiroot_fsolver_free (s);
  gsl_vector_free (x);
  return 0;
} 

