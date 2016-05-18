#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_multiroots.h>
#include <steam_pT.h>
#include <steam_ph.h>

struct rparams
  {
    double m[4];
    double p[4];
    double t[4];
  };

int mixer_f (const gsl_vector * x, void *params, 
              gsl_vector * f)
{
  double m[4];
  double p[4]; 
  double t[4]; 
  memcpy(m,((struct rparams *) params)->m,sizeof(m));
  memcpy(p,((struct rparams *) params)->p,sizeof(p));
  memcpy(t,((struct rparams *) params)->t,sizeof(t));

  int i;
  double X[16];
  double Y[16];

  for (i=0;i<=15;i++) {
      X[i] = gsl_vector_get (x, i);
  }

// setup equations for components
Y[0] = X[1] - X[0];
Y[1] = X[13] - X[12];
Y[2] = 0.96 * X[4] - X[5];

Y[3] = X[1] + X[2] - X[3];
Y[4] = X[5] - X[7];
Y[5] = X[5] - X[6];
Y[6] = X[1] * X[13] + X[2] * X[14] - X[3] * X[15]; 


// add steamtable lookups
SteamState S1 = freesteam_set_pT(X[4]*1e5, X[8]+273.15);
Y[7] = freesteam_h(S1)/1e3 - X[12];

SteamState S2 = freesteam_set_pT(X[5]*1e5, X[9]+273.15);
Y[8] = freesteam_h(S2)/1e3 - X[13];

SteamState S3 = freesteam_set_pT(X[6]*1e5, X[10]+273.15);
Y[9] = freesteam_h(S3)/1e3 - X[14];

SteamState S4 = freesteam_set_pT(X[7]*1e5, X[11]+273.15);
Y[10] = freesteam_h(S4)/1e3 - X[15];

// add fixed parameters (boundary conditions)
Y[11] = X[0] - m[0];
Y[12] = X[4] - p[0];
Y[13] = X[8] - t[0];

Y[14] = X[2] - m[2];

Y[15] = X[10] - t[2];

  for (i=0;i<=15;i++) {
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

  const size_t n = 16;
  struct rparams p = {
      274.0, 0.0, 28.5, 0.0, 
      30.0, 0.0, 0.0, 0.0,
      40.5, 0.0, 70.7, 0.0};
  gsl_multiroot_function f = {&mixer_f, n, &p};

  double x_init[16] = {184.0, 184.0, 184.0, 184.0, 184.0, 184.0, 184.0, 184.0, 184.0, 184.0, 184.0, 184.0, 184.0, 184.0, 184.0, 184.0};
  gsl_vector *x = gsl_vector_alloc (n);

  for (i=0;i<=15;i++) {
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
        gsl_multiroot_test_residual (s->f, 1e-7);
    }
  while (status == GSL_CONTINUE && iter < 1000);

  printf ("status = %s\n", gsl_strerror (status));

  gsl_multiroot_fsolver_free (s);
  gsl_vector_free (x);
  return 0;
} 

int print_state (size_t iter, gsl_multiroot_fsolver * s)
{
  SteamState S = freesteam_set_ph(gsl_vector_get (s->x,7)*1e5, gsl_vector_get (s->x,15)*1e3);

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
