define(`counterY',`-1')dnl
define(`countY',`define(`counterY',incr(counterY))counterY')dnl
dnl
define(`counterN',`0')dnl
dnl define node counter
define(`in',`define(`counterN',incr(counterN))$1')dnl
define(`out',`define(`counterN',incr(counterN))$1')dnl
define(`hook',$1)
dnl
define(valve_rel_dX,
`Y[countY] = X[eval(($2-1)*4+0)] - X[eval(($1-1)*4+0)];
Y[countY] = X[eval(($2-1)*4+2)] - X[eval(($1-1)*4+2)];
Y[countY] = $3 * X[eval(($1-1)*4+1)] - X[eval(($2-1)*4+1)];
')dnl
dnl
define(mixer,
`Y[countY] = X[eval(($1-1)*4+0)] + X[eval(($2-1)*4+0)] - X[eval(($3-1)*4+0)];
Y[countY] = X[eval(($1-1)*4+1)] + X[eval(($3-1)*4+1)]
Y[countY] = X[eval(($1-1)*4+1)] + X[eval(($2-1)*4+1)]
')dnl
dnl
dnl forloop from m4 examples
define(`forloop', `pushdef(`$1', `$2')_forloop($@)popdef(`$1')')dnl
define(`_forloop',
       `$4`'ifelse($1, `$3', `', `define(`$1', incr($1))$0($@)')')dnl
dnl
dnl
define(force,
`Y[countY] = X[eval(($1-1)*4+0)] - $2[eval($1-1)];
ifelse($3,p,`Y[countY] = X[eval(($1-1)*4+1)] - $3[eval($1-1)];')
ifelse($4,t,`Y[countY] = X[eval(($1-1)*4+3)] - $4[eval($1-1)];')
ifelse($5,h,`Y[countY] = X[eval(($1-1)*4+2)] - $5[eval($1-1)];')
')dnl
dnl
// setup equations for components
valve_rel_dX( in(1), out(2), 0.96)
mixer( hook(2), in(3), out(4))

// add steamtable lookups
forloop(`i', `1', counterN, `Y[countY] = freesteam_set_pT(S)/1e3 - X[eval(( i-1)*4+2)] 
')

// add fixed parameters (boundary conditions)
force(1,m,p,t)
