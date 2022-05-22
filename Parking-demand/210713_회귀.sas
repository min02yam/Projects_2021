FILENAME REFFILE '/home/alsdud55430/train0712(2).csv';  
PROC IMPORT DATAFILE=REFFILE
DBMS=CSV  
OUT=fi;  
GETNAMES=YES;
RUN;



proc reg data=fi ;
model target =code totpop	region	proplive	sub	bus	numpark	totrent	
a15	a20	a30	a40	a50	a60	a70	a80	a100	proprent	numpark_fam	type	
pub10	pub50	pub99	gukmin	forever	sanga	happy	
A	C	D	E	FMO	G	HI	J	K	L	N	L10F	L10M	
F10	M10	F20	M20	F30	M30	F40	M40	F50	M50	F60	M60	F70	M70	F80	M80	F90	M90	F100	M100
	fee_deposit	fee_rent 
 /selection= stepwise maxstep=1 details;
run;


/*62개 중 7개 제외*/
/*totpop	region	proplive	bus	numpark	totrent	
a15	a20	a30	a40	a50	a60	a70	a80	a100	proprent	numpark_fam	type	
pub10	pub50	pub99	gukmin	forever	sanga	happy	
A	C	D			HI	J	K	L		L10F	L10M	
F10	M10	F20	M20	F30	M30	F40	M40	F50	M50	F60	M60	F70	M70	F80	M80	F90		F100	
	fee_deposit	fee_rent */

proc REG data=fi ;
model target=totpop	region	proplive	bus	numpark	totrent	
a15	a20	a30	a40	a50	a60	a70	a80	a100	proprent	numpark_fam	type	
pub10	pub50	pub99	gukmin	forever	sanga	happy	
A	C	D			HI	J	K	L		L10F	L10M	
F10	M10	F20	M20	F30	M30	F40	M40	F50	M50	F60	M60	F70	M70	F80	M80	F90		F100	
	fee_deposit	fee_rent
/selection=backward sls=0.001;  
run;  


proc REG data=fi ;
model target=totpop	region	proplive	bus	numpark	totrent	
a15	a20	a30	a40	a50	a60	a70	a80	a100	proprent	numpark_fam	type	
pub10	pub50	pub99	gukmin	forever	sanga	happy	
A	C	D			HI	J	K	L		L10F	L10M	
F10	M10	F20	M20	F30	M30	F40	M40	F50	M50	F60	M60	F70	M70	F80	M80	F90		F100	
	fee_deposit	fee_rent
/selection=stepwise sle=0.05 sls=0.05;  
run;  

/*0.005*/
proc REG data=fi;
model target= numpark
a20 a30 numpark_fam pub10 pub99 gukmin
L10M a40 a50 a60 M40 ;
output out=m_out pred=pred;
run;  

proc sort data=m_out;
by descending pred;
run;   








/*0.01*/
proc REG data=fi;
model target= numpark
a20 a30 numpark_fam
pub10 pub99
gukmin
L10F L10M M30 a40 a50 a60 M40 ;
output out=m_out2 pred=pred2;
run;  

/*0.05*/
proc REG data=fi;
model target= numpark_fam
pub10 pub99 gukmin L10F L10M F20 M20 F30
M30 F40 F50 M50 fee_rent a40 a50 a60 M40 ;
output out=m_out3 pred=pred3;
run;  


FILENAME REFFILE '/home/alsdud55430/sasuser.v94/test0712_3.csv';  
PROC IMPORT DATAFILE=REFFILE
DBMS=CSV  
OUT=fitest3;  
GETNAMES=YES;
RUN;


proc univariate data=m_out noprint;
var pred target;
output out=preddata ;
run;  



proc univariate data=m_out(where=(splitwgt=.)) noprint;
weight smp_wgt;
var pred CARAVAN;
output out=preddata sumwgt=sumwgt;
run;  




/**/
FILENAME REFFILE '/home/alsdud55430/sasuser.v94/merge.csv';  
PROC IMPORT DATAFILE=REFFILE
DBMS=CSV  
OUT=merge;  
GETNAMES=YES;
RUN;

proc REG data=merge;
model target= numpark
a20 a30 numpark_fam pub10 pub99 gukmin
L10M a40 a50 a60 M40 / noint  ;
output out=m_out2 pred=pred2;
run;  


proc univariate data=m_out1(where=(target=.)) noprint;
var pred1 target;
output out=preddata ;
run;  

proc REG data=merge;
model target= numpark
a20 a30 numpark_fam
pub10 pub99
gukmin L10F L10M M30 a40 a50 a60 M40 ;
output out=m_out10 pred=pred10;
run;  

proc univariate data=m_out10(where=(target=.)) noprint;
var pred10 target;
output out=preddata ;
run;  



FILENAME REFFILE '/home/alsdud55430/sasuser.v94/merge2.csv';  
PROC IMPORT DATAFILE=REFFILE
DBMS=CSV  
OUT=merge2;  
GETNAMES=YES;
RUN;

proc reg data=merge2 ;
model target = totpop	region	proplive	sub	bus	numpark	totrent	
a15	a20	a30	a40	a50	a60	a70	a80	a100	proprent	numpark_fam	type	
pub10	pub50	pub99	gukmin	forever	sanga	happy	
A	C	D	E	FMO	G	HI	J	K	L	N	
	fee_deposit	fee_rent 
 /selection= stepwise maxstep=1 details;
run;

/*totpop	region	proplive	bus	numpark	totrent	
a15	a20	a30	a40	a50	a60	a70	a80	a100	proprent	numpark_fam	type	
pub10	pub50	pub99	gukmin	forever	sanga	happy	
A	C	D		HI	J	K	L	
	fee_deposit	fee_rent */

proc REG data=merge2 ;
model target=totpop	region	proplive	bus	numpark	totrent	
a15	a20	a30	a40	a50	a60	a70	a80	a100	proprent	numpark_fam	type	
pub10	pub50	pub99	gukmin	forever	sanga	happy	
A	C	D		HI	J	K	L	
	fee_deposit	fee_rent
/selection=backward sls=0.01;  
run;  


proc REG data=merge2 ;
model target=totpop	region	proplive	bus	numpark	totrent	
a15	a20	a30	a40	a50	a60	a70	a80	a100	proprent	numpark_fam	type	
pub10	pub50	pub99	gukmin	forever	sanga	happy	
A	C	D		HI	J	K	L	
	fee_deposit	fee_rent
/selection=stepwise sle=0.01 sls=0.01;  
run;  

/*0.001*/
/*numpark
a30
pub10
gukmin
a40
a50
pub99
*/

proc REG data=merge;
model target= numpark
a30
pub10
gukmin
a40
a50
pub99 ;
output out=m_out2 pred=pred2;
run;  

proc REG data=merge2 ;
model target= numpark
a30
pub10
gukmin
a40
a50
pub99;
output out=m_out12 pred=pred12;
run;  


proc REG data=merge2 ;
model target= numpark
a20
a30
a60
numpark_fam
pub10
pub99
gukmin
a40
a50;
output out=m_out14 pred=pred14;
run;  

