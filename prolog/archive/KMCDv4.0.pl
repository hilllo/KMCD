%% ---------------------------------------------------
%% FACTS that optimize the KBMC
%% ---------------------------------------------------
%% current student
%% cstu(jack).

%% previous student
pstu(mike).
pstu(hanks).
pstu(tom).
pstu(robbie).

%% current courses
%% cc(maths).
%% cc(physics).
%% cc(english).

%% future courses
fc(os).
fc(cpp).
fc(java).
fc(ai).
fc(ca).
fc(unix).
fc(ed).
fc(me).
%% ---------------------------------------------------
%% KEY FACTS to KBMC
%% ---------------------------------------------------
%% current students' marks
cstu_get_mark(jack,maths,85).
cstu_get_mark(jack,physics,76).

%% privious students' marks
%% mike
pstu_get_mark(mike,maths,80).
pstu_get_mark(mike,physics,75).
%% hanks
pstu_get_mark(hanks,maths,86).
pstu_get_mark(hanks,physics,72).
%% tom
pstu_get_mark(tom,maths,88).
pstu_get_mark(tom,physics,75).
%% robbie
pstu_get_mark(robbie,maths,85).
pstu_get_mark(robbie,physics,75).


%% privious students' course
%% mike
pstu_take_course(mike,os).
pstu_take_course(mike,cpp).
pstu_take_course(mike,java).
pstu_take_course(mike,ai).
pstu_take_course(mike,ca).
pstu_take_course(mike,ed).
%% hanks
pstu_take_course(hanks,os).
pstu_take_course(hanks,java).
pstu_take_course(hanks,unix).
pstu_take_course(hanks,ai).
pstu_take_course(hanks,ca).
pstu_take_course(hanks,ed).
%% tom
pstu_take_course(tom,me).
pstu_take_course(tom,cpp).
pstu_take_course(tom,ed).
%% robbie
pstu_take_course(robbie,me).
pstu_take_course(robbie,os).
pstu_take_course(robbie,cpp).
pstu_take_course(robbie,ca).

%% database
%% pstu_take_course(mike,database).
%% pstu_take_course(hanks,database).
%% pstu_take_course(tom,database).
%% %% operating_system
%% pstu_take_course(mike,operating_system).
%% pstu_take_course(hanks,operating_system).
%% pstu_take_course(robbie,operating_system).
%% %% c_plus_plus
%% pstu_take_course(tom,c_plus_plus).
%% %% ai
%% pstu_take_course(mike,ai).
%% pstu_take_course(robbie,ai).

%% privious students' passed course
%% mike
pstu_pass_course(mike,os).
pstu_pass_course(mike,java).
pstu_pass_course(mike,ai).
pstu_pass_course(mike,ca).
%% hanks
pstu_pass_course(hanks,os).
pstu_pass_course(hanks,java).
pstu_pass_course(hanks,unix).
pstu_pass_course(hanks,ed).
%% tom
pstu_pass_course(tom,me).
pstu_pass_course(tom,cpp).
pstu_pass_course(tom,ed).
%% robbie
pstu_pass_course(robbie,me).
pstu_pass_course(robbie,cpp).
pstu_pass_course(robbie,ca).
%% %% database
%% pstu_pass_course(mike,database).
%% pstu_pass_course(hanks,database).
%% pstu_pass_course(tom,database).
%% %% operating_system 
%% pstu_pass_course(mike,operating_system).
%% pstu_pass_course(hanks,operating_system).
%% pstu_pass_course(robbie,operating_system).
%% %% c_plus_plus
%% pstu_pass_course(tom,c_plus_plus).
%% %% ai
%% pstu_pass_course(mike,ai).
%% pstu_pass_course(robbie,ai).

%% major and courses groups
%% major(network_engineering,[java,operating_system],[database]).
%% major(software_engineering,[java,operating_syestem],[c_plus_plus]).

%% ---------------------------------------------------
%% KEY RULES to KBMC
%% ---------------------------------------------------
/*
FUNCTION:find out which courses student CSTU may pass
UPDATE:20140614
find_pstu/2:find privious students PSTU whose mark is similar to CSTU;
pass/2:		find out courses FC that student CSTU may pass
CSTU:	a current student that is going to choose a major;
PSTU:	a previous student from database;
FC:		the future courses that CSTU may pass;
C:		a courses that both CSTU and PSTU has chosen;
M1/M2:	mark of C;
*/
%% test(A,B,C):-get_mark(A,C,M1),get_mark(B,C,M2),A\=B,M2<M1+5.1,M2>M1-5.1.
similar_pstu(CSTU,PSTU):-
	cstu_get_mark(CSTU,C,M1),
	%% p_stu(PSTU),
	pstu_get_mark(PSTU,C,M2),
	%% CSTU\=PSTU,
	M2#=<M1+1,M2#>=M1-1.
pass(CSTU,FC):-
	setof(PSTU,similar_pstu(CSTU,PSTU),PSTUL),
	member(PSTU,PSTUL),
	pstu_pass_course(PSTU,FC).

similar_pstu_take_course(CSTU,PSTU,TC):-
	setof(PSTU,similar_pstu(CSTU,PSTU),PSTUL),
	member(PSTU,PSTUL),
	pstu_take_course(PSTU,TC).

similar_pstu_pass_course(CSTU,PSTU,PC):-
	setof(PSTU,similar_pstu(CSTU,PSTU),PSTUL),
	member(PSTU,PSTUL),
	pstu_pass_course(PSTU,PC).

/*
FUNCTION:calculate the supporting degree of the course FC
UPDATE:20140614
count/2:	count the number Count of the results of the predication P;
caltfc/2:	calculate the supporting degree TFC of the course FC of student CSTU;
P:		a specific predication;
Count:	the number of the results of P;
CSTU:	a current student that is going to choose a major;
FC:		the future courses that CSTU may pass;
TFC:	the supporting degree of FC;
*/
count(P,Count) :-
	findall(1,P,L),
	length(L,Count).

caltfc(CSTU,FC,TFC):-
	fc(FC),
	count(similar_pstu_pass_course(CSTU,_,FC),PTotal),
	count(similar_pstu_take_course(CSTU,_,FC),TTotal),
	TFC=PTotal/TTotal.
%% caltfc(CSTU,FC,TFC):-
%% 	fc(FC),
%% 	count(pass(CSTU,FC),CTotal),
%% 	count(pstu_take_course(_,FC),Total),
%% 	TFC=CTotal/Total.

%% choose_major(A,M):-major(M,C1,C2),pass(A,FC1),member(FC1,C1),pass(A,FC2),member(FC2,C2).