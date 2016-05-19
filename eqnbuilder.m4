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
include(model.m4)dnl
include(flags.m4)dnl
dnl steamtable lookups {{{1
forloop(`i',1,counterN,`SteamState S = freesteam_set_pT(X[eval(( i-1)*4+1)]*1e5, X[eval(( i-1)*4+3)]+273.15);
Y[countY] = freesteam_set_pT(S)/1e3 - X[eval(( i-1)*4+2)] 

')dnl
dnl }}}1
