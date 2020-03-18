*--------------------------------------------------

STAT 550 Final Project

*--------------------------------------------------;


* DATA;
proc import out=data 
  datafile='C:\Users\OWNER\Desktop\UFC Final Project\ufcdata.csv'
  dbms=csv replace;
run;

proc contents data=data;
run;

*--------------------------------------------------;

*
COVARIANCE AND CORRELATION BY POPULATION
	Population 1: y=fighter1
	Population 2: y=fighter2
;
proc sort data=data out=y_sort;
  by y;
run;

ods graphics on;
title 'Covariance and Correlation Matrices';
title2 'For each Population';
proc corr data=y_sort cov noprob sscp
plots=scatter(ellipse=confidence);
  var x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12; 
  by y notsorted;
run;
ods graphics off;

*--------------------------------------------------;

* 
NORMALITY ASSESSMENT
Chapter 4: The Multivariate Normal Distribution - Page 149
4.6 - Assessing the Assumption of Normality - Page 177
;
title 'Normality Assessment for x1, x2, x3, x4, x5, x6';
proc sgscatter data=data;
  matrix x1 x2 x3 x4 x5 x6 /diagonal=(histogram normal) 
  ellipse=(type=mean alpha=0.05) group=y transparency=0.5;
run;

title 'Normality Assessment for x7, x8, x9, x10, x11, x12';
proc sgscatter data=data;
  matrix x7 x8 x9 x10 x11 x12 /diagonal=(histogram normal) 
  ellipse=(type=mean alpha=0.05)group=y transparency=0.5;
run;

title 'Normality Assessment for Fighter Statistics Differences';
proc sgscatter data=data;
  matrix x3 x4 x5 x6 x7 x8 x9 x10 / diagonal=(histogram kernel normal)
  ellipse=(type=mean alpha=0.05) group=y transparency=0.7;
run;

title 'Normality Assessment for Variables w/ Most Variance';
title2 'x4, x6, x8, x9, x11 Account For:';
title3 '97.3% of Variation in Data for Population 1';
title4 '97.4% of Variation in Data for Population 2';
proc sgscatter data=data;
  matrix x4 x6 x8 x9 x11 /diagonal=(histogram normal) 
  ellipse=(type=mean alpha=0.05)group=y transparency=0.5;
run;

* 
MULTIVARIATE NORMALITY ASSESSMENT
;
proc iml; 
  use data; read all into X; N=NROW(X); 
    One=SHAPE(1,N,1); 
    MeanVec=(One`*X)/N; 
    M=REPEAT(MeanVec, N, 1);
    Sigma=(X-M)`*(X-M)/(N-1);
    Dis=VECDIAG((X-M)*INV(Sigma)*(X-M)`);
    print MeanVec; print Sigma; print Dis;
  create d2 from Dis; 
  append from Dis;
run; quit; 

data data2; 
  merge data d2(rename=(COL1=d2)); 
run;

proc sort data=data2 out=sort_d2; 
  by d2; 
run;

proc contents data=sort_d2; 
run;
* ^^Tells us Number of observations = N;
 
data norm_p; set sort_d2; 
  N=3446;
    NN=(_N_-.5)/N;
    Z=PROBIT(NN);
    Xsq=CINV(NN, 12);
  drop Obs fighter1 fighter2 y me wc year 
       x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12;
run;

proc print data=norm_p (firstobs=3435); 
run; 
* ^^To see the number to order AXIS1 for the Xsq quantities;

title 'Chi-Sq Probability Plot';
proc gplot data=norm_p;
  plot d2*Xsq / HAXIS=AXIS1 VAXIS=AXIS2 VREF=0;
    AXIS1 label=('Chi-Square') order=0 to 50;
	AXIS2 label=('Squared Distance');
run; quit;

*--------------------------------------------------;

proc freq data=data; 
  tables y me wc;
run;

proc sort data=data out=me_sort;
  by me;
run;

proc corr data=me_sort cov noprob; 
  var x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12;
    by me; 
run;

*
MANOVA -> MULTIPLE COMPARISONS
CHAPTER 6
Wilks' Lambda - Page 303

Full Model;
title 'MANOVA - Full Model';
proc glm data=data; 
  class y me wc; 
  model x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 = 
        y|me|wc / nouni;
  manova h = ye|w|mc / printe printh;
run;quit;

* 
Reduced Model; 
title 'MANOVA - Reduced Model';
proc glm data=data; 
  class y me wc; 
  model x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 =
        y y*me y*wc y*me*wc / nouni;
  manova h = y y*me y*wc y*me*wc / printe printh;
run;quit;

title 'MANOVA';
proc glm data=data; 
  class y me wc; 
  model x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 = y / nouni;
  manova h = y / printe printh summary;
run;quit;

*
Multiple Comparisons;
ods graphics on;
title 'Multiple Comparisons';
proc glm data=data
plots=intplot(limits) plots=diffplot(center) 
plots=meanplot(clband); 
  class y me wc; 
  model x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 =
        y y*me y*wc y*me*wc / nouni;
  lsmeans y y*me / 
  tdiff adjust=tukey lines;
run;quit;
ods graphics off;

* Inferences on Mean Vectors;
ods graphics on;
title 'Mean Inferences';
proc glm data=data
plots=meanplot(clband) plots=all;
  class y me wc;
  model x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 =
        y me wc y*me y*wc y*me*wc / nouni;
  means y me y*me / 
  deponly tukey cldiff clm lines;
run;quit;
ods graphics off;

title 'MANOVA - Weight Classes';
proc glm data=data; 
  class y me wc; 
  model x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 = 
        wc / nouni;
  manova h = wc / printe printh;
run; quit;

title 'Multivariate Analysis of Variance';
proc glm data=data; 
  class y me wc; 
  model x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 = y me wc / nouni;
  manova h = y me wc / printe printh;
run; quit;

*--------------------------------------------------;

*
PRINCIPAL COMPONENT ANALYSIS
;
ods graphics on;
title 'Analysis of Covariance Matrix with PCA';
proc princomp 
data=data out=pcdata 
cov plots(ncomps=3)=all;
  var x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12;
  ods output Eigenvalues=eigval;
  ods output Eigenvectors=eigvec;
run;
ods graphics off;

proc gplot data=eigval;
  plot Eigenvalue*Number;
run;

proc gplot data=eigvec;
  plot prin2*prin1 / vref=0 href=0;
  symbol1 C=black V=dot I=none PointLabel=('#Variable');
run;

proc gplot data=pcdata;
  plot prin2*prin1=1 prin3*prin1=1 prin3*prin2=1/ 
  vaxis=axis1 haxis=axis2 frame;
    axis1 order=(-8.0 to 8.0 by 1.0);
	axis2 order=(-8.0 to 8.0 by 1.0);
	symbol1 C=black V=dot I=none PointLabel=("#y");
run; quit;

ods graphics on;
proc corr data=pcdata noprob
plots=scatter(ellipse=prediction);
  var x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12;
  with prin1 prin2 prin3;
run;
ods graphics off;

ods graphics on;
proc corr data=pcdata noprob
plots=scatter(ellipse=prediction);
  var x4 x6 x8 x9 x11;
  with prin1 prin2 prin3;
run;
ods graphics off;

data pcdata; set pcdata;
drop x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12;
run;

*--------------------------------------------------;

* Testing/Training Data;
proc sql; 
  create table pc_train as 
  select * from pcdata where year <> 2018;
quit;

proc sql; 
  create table pc_test as 
  select * from pcdata where year = 2018;
quit;

*
Discriminant Analysis 
Using Principal Components 
;
title 'Discriminant Analysis Using Principal Components';
proc discrim 
data=pc_train testdata=pc_test method=normal pool=yes
simple wcov pcov distance crossvalidate;
  class y; testclass y;
    var prin1 prin2 prin3;
    priors equal; 
run;

*--------------------------------------------------;

* Training/Testing Data; 
proc sql; 
  create table train as 
  select * from data where year <> 2018;
quit;
proc sql; 
  create table test as 
  select * from data where year = 2018;
quit;

*
Discriminant Analysis 
Using Original Variables 
;
title 'Discriminant Analysis for Full Data';
proc discrim 
data=train testdata=test method=normal pool=test
simple wcov pcov distance crossvalidate;
  class y;testclass y;
    var x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12;
    priors equal; 
run;

proc discrim 
data=train testdata=test method=normal pool=test
simple wcov pcov distance crossvalidate;
  class y;testclass y;
    var x2 x3 x4 x5 x6 x7 x8 x9 x11 x12;
    priors equal; 
run;

*--------------------------------------------------;
