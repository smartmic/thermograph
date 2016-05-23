dnl node counter
define(counterN,`0')dnl
define(in,`define(`counterN',incr(counterN))`in'`('$1`)'')dnl
define(out,`define(`counterN',incr(counterN))`out'`('$1`)'')dnl
dnl do not count connected nodes twice, therefore use other name
define(hook,$1)dnl
divert(-1)dnl
include(model.m4)dnl
pushdef(`in',$1)dnl
pushdef(`out',$1)dnl
include(eqnlib.m4)dnl
divert(0)dnl
dnl WRITE EQNS BEGINNING FROM NEXT LINE…
struct rparams
{
  double `m'[counterN];
  double `p'[counterN];
  double `t'[counterN];
};

int model_f (const gsl_vector * x, void *params, gsl_vector * f)
{

  double `m'[counterN];
  double `p'[counterN];
  double `t'[counterN];
  memcpy(`m',((struct rparams *) params)->`m', sizeof(`m')); 
  memcpy(`p',((struct rparams *) params)->`p', sizeof(`p')); 
  memcpy(`t',((struct rparams *) params)->`t', sizeof(`t')); 

  int i;

  double X[eval(counterN*3)];
  double Y[eval(counterN*3)];
  for (i=0; i<eval(counterN*3); i++) {
    X[i] = gsl_vector_get (x, i);
  }

  // START OF EQNS
include(model.m4)dnl
include(flags.m4)dnl
  // END OF EQNS

  for (i=0; i<eval(counterN*3); i++) {
    gsl_vector_set (f, i, Y[i]);
  }

  return GSL_SUCCESS;
}
