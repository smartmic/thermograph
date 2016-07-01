(let* ([filename "src/thermograph.h"]
       [p (open-output-file filename)])
  (display (string-append "
#define NN "(number->string (length xmap))"
#define NE "(number->string (length eqns))"

struct rparams
{
    double m[NN];
    double p[NN];
    double t[NN];
};

double h_pT(double *, double *);
double T_ph(double *, double *);

int model_f (const gsl_vector *, void *, gsl_vector *);
int model_df (const gsl_vector *, void *, gsl_matrix *);
int model_fdf (const gsl_vector *, void *, gsl_vector *, gsl_matrix *);
") p) (close-port p))
