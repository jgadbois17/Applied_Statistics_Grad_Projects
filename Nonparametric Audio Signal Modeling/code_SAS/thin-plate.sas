*--------------------------------------------------------------------------------

STAT 560 PROJECT 
MODELING THE PERIODIC VARIATIONS IN MUSIC SIGNALS 

METHODS: 
LOESS REGRESSION
THIN-PLATE SMOOTHING SPLINE 

*--------------------------------------------------------------------------------;

* 
Load Split Data 
; 
proc import out=df 
datafile='C:\Users\OWNER\Desktop\STAT_560\project\splitsample.csv'
dbms=csv replace; 
run;
data df; 
set df; drop VAR1; 
run;

proc import out=dfp
datafile='C:\Users\OWNER\Desktop\STAT_560\project\splitsample.csv'
dbms=csv replace; 
run; 
data dfp; 
set dfp; drop VAR1; 
run; 

* Graphing Parameters; 
symbol1 color=blue value=dot; 
symbol2 color=red value=none interpol=join line=1;
symbol3 color=red value=none interpol=join line=2;
symbol4 color=red value=none interpol=join line=2;

*--------------------------------------------------------------------------------;

* For Projects; 

proc tpspline data=df; 
  model y = (n) / m=2;
  output out=pm2 pred lclm uclm;
run;

title 'Thin-Plate Spline: M=2';
proc gplot data=pm2; 
  plot (y P_y LCLM_y UCLM_y)*n/ 
		overlay name='plotm2';
run;

*--------------------------------------------------------------------------------;

* 
  Thin-Plate Spline M=1
; 
proc tpspline data=df; 
  model y = (n) / m=1;
  output out=pm1 pred lclm uclm;
run;
title 'm=1';
proc gplot data=pm1; 
  plot (y P_y LCLM_y UCLM_y)*n/ overlay name='pm1';
run; 

* 
  Thin-Plate Spline M=2
; 
proc tpspline data=df; 
  model y = (n) / m=2;
  output out=pm2 pred lclm uclm; 
  score data=dfp out=pm2res;
run;
title 'm=2';
proc gplot data=pm2; 
  plot (y P_y LCLM_y UCLM_y)*n/ overlay name='pm2';
run; 

* 
  Thin-Plate Spline M=3
; 
proc tpspline data=df; 
  model y = (n) / m=3;
  output out=pm3 pred lclm uclm;
run;
title 'm=3';
proc gplot data=pm3; 
  plot (y P_y LCLM_y UCLM_y)*n/ overlay name='pm3';
run; 

*
  Plot 4
;
goption display;
proc greplay nofs tc=sashelp.templt template=l2r2;
igout gseg;
treplay 1:pm1 2:pm2 3:pm3;
run;

*--------------------------------------------------------------------------------;
