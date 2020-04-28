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

proc print data=df;
run; 

proc contents data=df; 
run;

* 
Load Full Data 
;
proc import out=dfp
datafile='C:\Users\OWNER\Desktop\STAT_560\project\splitsample.csv'
dbms=csv replace; 
run; 

data dfp; 
set dfp; drop VAR1; 
run; 

proc print data=dfp;
run;

*--------------------------------------------------------------------------------;

* Graphing Parameters; 
symbol1 color=blue value=dot; 
symbol2 color=red value=none interpol=join line=1;
symbol3 color=red value=none interpol=join line=2;
symbol4 color=red value=none interpol=join line=2;

* 
Scatter plot 
;
proc gplot data=df; 
  plot y*n; 
run;

*--------------------------------------------------------------------------------;

* 
  AICC Optimal Loess Curve
; 
proc loess data=df; 
  model y = n / degree=1 clm; 
  ods output OutputStatistics=results1;
  score data=dfp;
run; 

*--------------------------------------------------------------------------------;

* For Project; 

proc loess data=df; 
  model y = n / 
		degree=1 
		clm 
		smooth=0.01 0.02 0.03 0.04; 
  ods output OutputStatistics=result1;
run; 

proc gplot data=result1; 
  by SmoothingParameter; 
  plot (DepVar Pred LowerCL UpperCL)*n / 
	    overlay name='graph';
run;


*--------------------------------------------------------------------------------;

* 
  Loess Curves 
  Smoothing Parameters: 0.01
  Degree 1
;
proc loess data=df; 
  model y = n / degree=1 clm smooth=0.01; 
  ods output OutputStatistics=result1;
  score data=dfp;
run; 

title 'Linear Loess | SP = 0.01';
proc gplot data=result1; 
  by SmoothingParameter; 
  plot (DepVar Pred LowerCL UpperCL)*n/ overlay name='pL1';
run; 

* 
  Loess Curves 
  Smoothing Parameters: 0.02
  Degree 1
;
proc loess data=df; 
  model y = n / degree=1 clm smooth=0.02; 
  ods output OutputStatistics=result2;
  score data=dfp;
run; 

title 'Linear Loess | SP = 0.02';
proc gplot data=result2; 
  by SmoothingParameter; 
  plot (DepVar Pred LowerCL UpperCL)*n/ overlay name='pL2';
run;

* 
  Loess Curves 
  Smoothing Parameters: 0.03
  Degree 1
;
proc loess data=df; 
  model y = n / degree=1 clm smooth=0.03; 
  ods output OutputStatistics=result3;
  score data=dfp;
run; 

title 'Linear Loess | SP = 0.03';
proc gplot data=result3; 
  by SmoothingParameter; 
  plot (DepVar Pred LowerCL UpperCL)*n/ overlay name='pL3';
run;

* 
  Loess Curves 
  Smoothing Parameters: 0.04
  Degree 1
;
proc loess data=df; 
  model y = n / degree=1 clm smooth=0.04; 
  ods output OutputStatistics=result4;
  score data=dfp;
run; 

title 'Linear Loess | SP = 0.04';
proc gplot data=result4; 
  by SmoothingParameter; 
  plot (DepVar Pred LowerCL UpperCL)*n/ overlay name='pL4';
run;

*
  Plot All 4
;
goptions display; 
proc greplay nofs tc=sashelp.templt template=l2r2;
  igout gseg; 
  treplay 1:pL1 2:pL2 3:pL3 4:pL4;
run; 

*--------------------------------------------------------------------------------;

* 
  AICC Optimal Loess Curve
; 
proc loess data=df; 
  model y = n / degree=2 clm; 
  ods output OutputStatistics=results2;
  score data=dfp;
run;

*--------------------------------------------------------------------------------;

* 
  Loess Curves 
  Smoothing Parameters: 0.01 
  Degree 2
;
proc loess data=df; 
  model y = n / degree=2 clm smooth=0.01; 
  ods output OutputStatistics=result11;
  score data=dfp;
run; 

title 'Linear Loess | SP = 0.01';
proc gplot data=result11; 
  by SmoothingParameter; 
  plot (DepVar Pred LowerCL UpperCL)*n/ overlay name='pL11';
run;

* 
  Loess Curves 
  Smoothing Parameters: 0.02 
  Degree 2
;
proc loess data=df; 
  model y = n / degree=2 clm smooth=0.02; 
  ods output OutputStatistics=result22;
  score data=dfp;
run; 

title 'Linear Loess | SP = 0.02';
proc gplot data=result22; 
  by SmoothingParameter; 
  plot (DepVar Pred LowerCL UpperCL)*n/ overlay name='pL22';
run;

* 
  Loess Curves 
  Smoothing Parameters: 0.03
  Degree 2
;
proc loess data=df; 
  model y = n / degree=2 clm smooth=0.03; 
  ods output OutputStatistics=result33;
  score data=dfp;
run; 

title 'Linear Loess | SP = 0.03';
proc gplot data=result33; 
  by SmoothingParameter; 
  plot (DepVar Pred LowerCL UpperCL)*n/ overlay name='pL33';
run;

* 
  Loess Curves 
  Smoothing Parameters: 0.04
  Degree 2
;
proc loess data=df; 
  model y = n / degree=2 clm smooth=0.04; 
  ods output OutputStatistics=result44;
  score data=dfp;
run; 

title 'Linear Loess | SP = 0.04';
proc gplot data=result44; 
  by SmoothingParameter; 
  plot (DepVar Pred LowerCL UpperCL)*n/ overlay name='pL44';
run;

*
  Plot All 4
;
goptions display; 
proc greplay nofs tc=sashelp.templt template=l2r2;
  igout gseg; 
  treplay 1:pL11 2:pL22 3:pL33 4:pL44;
run; 

*--------------------------------------------------------------------------------;


