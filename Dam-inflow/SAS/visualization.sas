data a;
infile "/home/alsdud55430/sasuser.v94/0902(3).csv" dlm=',';
input 홍수사상번호	연	월	일	시간	유입량 ;
date=intnx('day', '10jul06'd, _n_-1);
format date date7.;
run;



proc sgplot;
ods graphics on / width=15in height=3in;
series x=date y=유입량/ legendlabel="series";
refline '01JUN07'd/axis=x; refline '24SEP07'd/axis=x; refline '27DEC07'd/axis=x; 
refline '14OCT08'd/axis=x; refline '08JUN09'd/axis=x; refline '18MAY11'd/axis=x; 
refline '07OCT12'd/axis=x; refline '24NOV13'd/axis=x; refline '09JUN14'd/axis=x; 
yaxis label="유입량";
xaxis label="date" display= none;
run;










refline '1jan09'd/axis=x; refline '1jan10'd/axis=x; refline '1jan11'd/axis=x; 
refline '1jan12'd/axis=x; refline '1jan13'd/axis=x; refline '1jan14'd/axis=x; 
refline '1jan15'd/axis=x; refline '1jan16'd/axis=x; refline '1jan17'd/axis=x; 
refline '1jan18'd/axis=x; refline '1jan19'd/axis=x; refline '1jan20'd/axis=x; 




scatter x=date y=z/ legendlabel="actual";
