*--------------------------------------------------

STAT 530 
Experimental Design & Analysis
Final Project

Factors: 
A = Average mins sleep/day 
B = Average mins social media/day
C = Average mins commute to school
D = Average mins studying math/week

Response: 
Y = Test scores out of 50

*--------------------------------------------------;

* Design;
data design;
  do C = -1 to 1 by 2; 
  do B = -1 to 1 by 2; 
  do A = -1 to 1 by 2;
     D = A*B*C;
	input Run$ @@;
	output;
  end; end; end;
cards;
(1) ad bd ab cd ac bc abcd
;
run;

proc sql; 
  create table design2 as
    select A, B, C, D, Run 
    from design; 
quit;

proc print data=design2;
run;

* Design Matrix with Data;
data layout;
  do C = -1 to 1 by 2; 
  do B = -1 to 1 by 2; 
  do A = -1 to 1 by 2; 
     D = A*B*C;
    input Run$ y @@;
    output;
  end; end; end;
cards;
(1) 45.0 ad 39.5 bd 42.5 ab 43.0
cd 31.0 ac 37.0 bc 25.0 abcd 40.0
;
run;

* Define Interaction Terms;
data inter;
set layout;
  AB = A*B; 
  AC = A*C; 
  AD = A*D;
run;

proc sql; 
  create table data as 
    select A, B, C, D, AB, AC, AD, Run, y 
    from inter; 
quit; 

title 'One-Half Fractional Factorial Design for 4 Factors';
title2 'Defining Relation: I = ABCD';
proc print data=data;
run;

* Estimate Effects;
title 'Estimating Effects';
proc glm data=data;
  class A B C D AB AC AD;
  model y = A B C D AB AC AD / aliasing;
    estimate 'A' A -1 1; 
    estimate 'B' B -1 1; 
	estimate 'C' C -1 1; 
    estimate 'D' D -1 1;
	estimate 'AB' AB -1 1; 
    estimate 'AC' AC -1 1;
    estimate 'AD' AD -1 1;
run; quit;

* Normality Assessment;
proc rank normal=vw; 
  var y;  
  ranks nscore;
run;

title 'Assessing Normality of Y';
proc gplot; 
  plot y*nscore; 
label nscore='Normal Score';
run; quit;

*--------------------------------------------------;

* Reduced Model Option 1 - INCLUDING AB;
title 'The Reduced Model';
proc glm data=data;
  class A B C D AB AC AD; 
  model y = A C AB AC;
    estimate 'A' A -1 1; 
    estimate 'C' C -1 1;
	estimate 'AB' AB -1 1; 
    estimate 'AC' AC -1 1;
run; quit;

* Reduced Model Option 2 - EXLUDING AB;
title 'The Reduced Model';
proc glm data=data;
  class A B C D AB AC AD; 
  model y = A C AC;
    estimate 'A' A -1 1; 
    estimate 'C' C -1 1;
	estimate 'AC' AC -1 1;
run; quit;

* Reduced Model Option 3 - INCLUDING AB AND B;
title 'The Reduced Model';
proc glm data=data;
  class A B C D AB AC AD; 
  model y = A B C AB AC;
    estimate 'A' A -1 1; 
    estimate 'B' B -1 1; 
    estimate 'C' C -1 1; 
	estimate 'AB' AB -1 1; 
    estimate 'AC' AC -1 1;
run; quit;

* Reduced Model Option 4;
title 'The Reduced Model';
proc glm data=data;
  class A B C D AB AC AD; 
  model y = A C D AB AC;
    estimate 'A' A -1 1; 
    estimate 'B' B -1 1; 
    estimate 'C' C -1 1; 
	estimate 'AB' AB -1 1; 
    estimate 'AC' AC -1 1;
run; quit;

* Reduced Model Option 4;
title 'The Reduced Model';
proc glm data=data;
  class A B C D AB AC AD; 
  model y = A C D AB AC;
    estimate 'A' A -1 1; 
    estimate 'B' B -1 1; 
    estimate 'C' C -1 1; 
	estimate 'AB' AB -1 1; 
    estimate 'AC' AC -1 1;
run; quit;

* Final Model;
title 'The Reduced Model';
proc glm data=data;
  class A B C D AB AC AD; 
  model y = A B C AB AC;
    estimate 'A' A -1 1; 
    estimate 'B' B -1 1; 
    estimate 'C' C -1 1; 
	estimate 'AB' AB -1 1; 
    estimate 'AC' AC -1 1;
run; quit;

*--------------------------------------------------;
 
* Tukey's Multiple Comparison Procedures;
ods graphics on;
title "Tukey's Multiple Comparison Procedures";
proc glm data=data
plots=(diffplot(center) controlplot intplot(limits) meanplot(clband));
  class A B C D AB AC AD;
  model y = A B C A*B A*C;
  lsmeans A|C A|B / tdiff lines stderr adjust=tukey out=lsm;
run; quit;
ods graphics off;

*--------------------------------------------------;

ods graphics on;
title "Tukey's Multiple Comparison Procedures";
proc glm data=data
plots=(diffplot(center) controlplot intplot(limits) meanplot(clband));
  class A B C D AB AC AD;
  model y = A C A*C ;
  lsmeans A|C / tdiff lines stderr adjust=tukey out=lsm;
run; quit;
ods graphics off;

ods graphics on;
title "Tukey's Multiple Comparison Procedures";
proc glm data=data
plots=(diffplot(center) controlplot intplot(limits) meanplot(clband));
  class A B C D AB AC AD;
  model y = A B A*B ;
  lsmeans A|B / tdiff lines stderr adjust=tukey out=lsm;
run; quit;
ods graphics off;

*--------------------------------------------------;
