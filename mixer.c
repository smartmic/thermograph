#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_multiroots.h>
#include <steam_pT.h>

struct rparams
  {
    double m[4];
    double t[4];
    double p[4];
    double h[4];
  };

int mixer_f (const gsl_vector * x, void *params, 
              gsl_vector * f)
{
  double m[4];
  double t[4]; 
  double p[4]; 
  double h[4]; 
  memcpy(m,((struct rparams *) params)->m,sizeof(m));
  memcpy(t,((struct rparams *) params)->t,sizeof(t));
  memcpy(p,((struct rparams *) params)->p,sizeof(p));
  memcpy(h,((struct rparams *) params)->h,sizeof(h));

  int i;
  double X[2];
  double Y[2];

  for (i=0;i<=1;i++) {
      X[i] = gsl_vector_get (x, i);
  }

  Y[0] = m[0] * (1 - X[0]);
  Y[1] = t[0] * (X[1] - X[0] * X[0]);

  for (i=0;i<=1;i++) {
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

  const size_t n = 2;
  struct rparams p = {1.0, 1.0, 1.0, 1.0, 10.0, 10.0, 10.0, 10.0};
  gsl_multiroot_function f = {&mixer_f, n, &p};

  double x_init[2] = {-10.0, -5.0};
  gsl_vector *x = gsl_vector_alloc (n);

  gsl_vector_set (x, 0, x_init[0]);
  gsl_vector_set (x, 1, x_init[1]);

  T = gsl_multiroot_fsolver_hybrids;
  s = gsl_multiroot_fsolver_alloc (T, 2);
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
  printf ("iter = %3u x = % .3f % .3f "
          "f(x) = % .3e % .3e\n",
          iter,
          gsl_vector_get (s->x, 0), 
          gsl_vector_get (s->x, 1),
          gsl_vector_get (s->f, 0), 
          gsl_vector_get (s->f, 1));
}
