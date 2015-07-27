%% ---------------------------------------------------
%% FACTS that optimize the KBMC
%% ---------------------------------------------------
%% current_student
%% current_student(jack).

%% previous student
previous_student(tom).
previous_student(mike).
previous_student(rose).
previous_student(lily).

%% current courses
%% current_course(maths).
%% current_course(physics).
%% current_course(english).

%% future courses
future_course(database).
future_course(operating_system).
future_course(c_plus_plus).
future_course(ai).

%% ---------------------------------------------------
%% KEY FACTS to KBMC
%% ---------------------------------------------------
%% current students' marks
get_mark(jack,maths,80).
get_mark(jack,physics,80).
get_mark(jack,english,80).

%% privious students' marks
%% tom is similar to jack
get_mark(tom,maths,80).				%is similar to jack
get_mark(tom,physics,90).
get_mark(tom,english,85).			%is similar to jack
%% mike is similar to jack
get_mark(mike,maths,85).			%is similar to jack
get_mark(mike,physics,85).			%is similar to jack
get_mark(mike,english,85).			%is similar to jack
%% rose
get_mark(rose,maths,100).
get_mark(rose,physics,100).
get_mark(rose,english,100).
%% lily is similar to jack
get_mark(lily,maths,70).
get_mark(lily,physics,80).			%is similar to jack
get_mark(lily,english,70).

%% privious students' course
%% database 						2/3
take_course(tom,database).			%is similar to jack
take_course(mike,database).			%is similar to jack
take_course(rose,database).			
%% operating_system 				3/3
take_course(tom,operating_system).	%is similar to jack
take_course(mike,operating_system).	%is similar to jack
take_course(lily,operating_system).	%is similar to jack
%% c_plus_plus						0/1
take_course(rose,c_plus_plus).		
%% ai								2/2
take_course(tom,ai).				%is similar to jack
take_course(lily,ai).				%is similar to jack

%% major and courses groups
%% major(network_engineering,[java,operating_system],[database]).
%% major(software_engineering,[java,operating_syestem],[c_plus_plus]).

%% ---------------------------------------------------
%% KEY RULES to KBMC
%% ---------------------------------------------------
/*
FUNCTION:find out which courses student STU may pass
UPDATE:20140614
find_pstu/2:find privious students PSTU whose mark is similar to STU;
pass/2:		find out courses FC that student STU may pass
STU:	a current student that is going to choose a major;
PSTU:	a previous student from database;
FC:		the future courses that STU may pass;
C:		a courses that both STU and PSTU has chosen;
M1/M2:	mark of C;
*/
%% test(A,B,C):-get_mark(A,C,M1),get_mark(B,C,M2),A\=B,M2<M1+5.1,M2>M1-5.1.
find_pstu(STU,PSTU):-
	get_mark(STU,C,M1),
	previous_student(PSTU),
	get_mark(PSTU,C,M2),
	%% STU\=PSTU,
	M2<M1+6,M2>M1-6.
pass(STU,FC):-
	setof(PSTU,find_pstu(STU,PSTU),PSTUL),
	member(PSTU,PSTUL),
	take_course(PSTU,FC).

/*
FUNCTION:calculate the supporting degree of the course FC
UPDATE:20140614
count/2:	count the number Count of the results of the predication P;
caltfc/2:	calculate the supporting degree TFC of the course FC of student STU;
P:		a specific predication;
Count:	the number of the results of P;
STU:	a current student that is going to choose a major;
FC:		the future courses that STU may pass;
TFC:	the supporting degree of FC;
*/
count(P,Count) :-
	findall(1,P,L),
	length(L,Count).
caltfc(STU,FC,TFC):-
	future_course(FC),
	count(pass(STU,FC),CTotal),
	count(take_course(_,FC),Total),
	TFC=CTotal/Total.

%% choose_major(A,M):-major(M,C1,C2),pass(A,FC1),member(FC1,C1),pass(A,FC2),member(FC2,C2).