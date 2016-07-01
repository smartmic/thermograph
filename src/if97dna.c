extern void steam97_ (
        double *,
        double *,
        double *,
        double *,
        double *,
        double *,
        double *,
        int *,
        int *,
        double *,
        int *,
        int *,
        int *);

double h_pT (double *p, double *T) {

  double h=1000;
  double s=1000;
  double v=1000;
  double x=1000;
  double u=1000;
  int in1=1;
  int in2=3;
  double epsva=1e-8;
  int ivmax=20;
  int fiter=0;
  int fiter0=0;

  steam97_ (p,&h,T,&s,&v,&x,&u,&in1,&in2,&epsva,&ivmax,&fiter,&fiter0);

  return h;
}

double T_ph (double *p, double *h) {

  double T=1000;
  double s=1000;
  double v=1000;
  double x=1000;
  double u=1000;
  int in1=1;
  int in2=2;
  double epsva=1e-8;
  int ivmax=20;
  int fiter=0;
  int fiter0=0;

  steam97_ (p,h,&T,&s,&v,&x,&u,&in1,&in2,&epsva,&ivmax,&fiter,&fiter0);

  return T;

}
