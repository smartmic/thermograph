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
dnl node lookup table {{{2
define(m,`eval'($1-1))
define(p,`eval'(counterN*1+$1-1))
define(t,`eval'(counterN*2+$1-1))
define(h,`eval'(counterN*3+$1-1))
dnl }}}2
dnl }}}1
dnl component definitions {{{1
dnl valve with relative pressure drop {{{2
define(valve_rel_dX,`  Y[countY] = X[m($2)] - X[m($1)];
  Y[countY] = X[h($2)] - X[h($1)];
  Y[countY] = $3 * X[p($1)] - X[p($2)];
')
dnl }}}2
dnl mixer {{{2
define(mixer,`  Y[countY] = X[m($1)] + X[m($2)] - X[m($3)];
  Y[countY] = X[p($1)] - X[p($3)];
  Y[countY] = X[p($1)] - X[p($2)];
  Y[countY] = X[m($1)]*X[h($1)] + X[m($2)]*X[h($2)] - X[m($3)]*X[h($3)]; 
')
dnl }}}2
dnl }}}1
dnl boundary conditions {{{1
define(force,`ifelse($2,m,`  Y[countY] = X[m($1)] - `m'[eval($1-1)];
')ifelse($3,p,`  Y[countY] = X[p($1)] - `p'[eval($1-1)];
')ifelse($4,t,`  Y[countY] = X[t($1)] - `t'[eval($1-1)];
')ifelse($5,h,`  Y[countY] = X[h($1)] - `h'[eval($1-1)];')')dnl
dnl }}}1
