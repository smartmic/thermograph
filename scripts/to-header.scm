;;; to-header.scm
;;; Template to C header file (not to confuse with component
;;; "header")
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
