#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_multiroots.h>
#include <steam_pT.h>
#include <steam_ph.h>

struct rparams
  {
    double m[2];
    double t[2];
    double p[1];
  };

int mixer_f (const gsl_vector * x, void *params, 
              gsl_vector * f)
{
  double m[2];
  double t[2]; 
  double p[1]; 
  memcpy(m,((struct rparams *) params)->m,sizeof(m));
  memcpy(t,((struct rparams *) params)->t,sizeof(t));
  memcpy(p,((struct rparams *) params)->p,sizeof(p));

  int i;
  double X[9];
  double Y[9];

  for (i=0;i<=8;i++) {
      X[i] = gsl_vector_get (x, i);
  }

  Y[0] = 274.0 - X[0];
  Y[1] = 0.952381 * 30.0 - X[2];

  SteamState S0 = freesteam_set_pT(X[2]*1e5,40.5+273.15);
  Y[2] = freesteam_h(S0)/1e3 - X[5];
  Y[3] = 28.5 + X[0] - X[1];
  Y[4] = X[2] - X[3];
  Y[5] = X[2] - X[4];
  

  Y[6] = 28.5 * X[7] + X[0]*X[5] - X[1]*X[6];

  SteamState S1 = freesteam_set_pT(X[3]*1e5,70.7+273.15);
  Y[7] = freesteam_h(S1)/1e3 - X[7];

  SteamState S2 = freesteam_set_pT(30.0*1e5,40.5+273.15);
  Y[8] = freesteam_h(S2)/1e3 - X[8];


  for (i=0;i<=8;i++) {
      gsl_vector_set (f, i, Y[i]);
  }

  return GSL_SUCCESS;
}

int main (void)
{
  const gsl_multiroot_fsolver_type *T;
  gsl_multiroot_fsolver *s;

  int status;
  size_t i, iter = 0;

  const size_t n = 9;
  struct rparams p = {28.5, 274.0, 70.7, 40.5, 30.0};
  gsl_multiroot_function f = {&mixer_f, n, &p};

  double x_init[9] = {100.0, 100.0, 1.0, 1.0, 1.0, 100.0, 100.0, 100.0, 100.0};
  gsl_vector *x = gsl_vector_alloc (n);

  for (i=0;i<=8;i++) {
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
        gsl_multiroot_test_residual (s->f, 1e-8);
    }
  while (status == GSL_CONTINUE && iter < 1000);

  printf ("status = %s\n", gsl_strerror (status));

  gsl_multiroot_fsolver_free (s);
  gsl_vector_free (x);
  return 0;
} 

int print_state (size_t iter, gsl_multiroot_fsolver * s)
{
  SteamState S = freesteam_set_ph(gsl_vector_get (s->x,4)*1e5, gsl_vector_get (s->x,6)*1e3);

  printf ("iter = %3u x = % .3f % .3f "
          "f(x) = % .3e % .3e "
          "T4 = %.3f\n",
          iter,
          gsl_vector_get (s->x, 0), 
          gsl_vector_get (s->x, 1),
          gsl_vector_get (s->f, 0), 
          gsl_vector_get (s->f, 1),
          freesteam_T(S)-273.15);
}
