// if97dna.c
// Interface to call water-steam functions from an external IF97
// library.
// 
// This file is part of Thermograph.
// 
// Thermograph is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
// 
// Thermograph is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with Thermograph. If not, see <http://www.gnu.org/licenses/>.
// 
// Copyright 2016 Martin Michel

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
