dnl util macros {{{1
dnl forloop from m4 examples {{{2
define(forloop, `pushdef(`$1', `$2')_forloop($@)popdef(`$1')')dnl
define(`_forloop',
       `$4`'ifelse($1, `$3', `', `define(`$1', incr($1))$0($@)')')dnl
dnl }}}2
dnl counter for number of equations {{{2
define(counterY,`-1')dnl
define(countY,`define(`counterY',incr(counterY))counterY')dnl
dnl }}}2
dnl }}}1
dnl component definitions {{{1
dnl valve with relative pressure drop {{{2
define(valve_rel_dX,
`Y[countY] = X[eval(($2-1)*4+0)] - X[eval(($1-1)*4+0)];
Y[countY] = X[eval(($2-1)*4+2)] - X[eval(($1-1)*4+2)];
Y[countY] = $3 * X[eval(($1-1)*4+1)] - X[eval(($2-1)*4+1)];
')dnl
dnl }}}2
dnl mixer {{{2
define(mixer,
`Y[countY] = X[eval(($1-1)*4+0)] + X[eval(($2-1)*4+0)] - X[eval(($3-1)*4+0)];
Y[countY] = X[eval(($1-1)*4+1)] + X[eval(($3-1)*4+1)];
Y[countY] = X[eval(($1-1)*4+1)] + X[eval(($2-1)*4+1)];
Y[countY] = X[eval(($1-1)*4+0)] * X[eval(($1-1)*4+2)] + X[eval(($2-1)*4+0)] * X[eval(($2-1)*4+2)] - X[eval(($3-1)*4+0)] * X[eval(($3-1)*4+2)]; 
')dnl
dnl }}}2
dnl }}}1
dnl boundary conditions {{{1
define(force,
`Y[countY] = X[eval(($1-1)*4+0)] - $2[eval($1-1)];
ifelse($3,p,`Y[countY] = X[eval(($1-1)*4+1)] - $3[eval($1-1)];')
ifelse($4,t,`Y[countY] = X[eval(($1-1)*4+3)] - $4[eval($1-1)];')
ifelse($5,h,`Y[countY] = X[eval(($1-1)*4+2)] - $5[eval($1-1)];')
')dnl
dnl }}}1
