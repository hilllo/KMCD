%% ---------------------------------------------------
%% KEY FACTS to KBMC
%% ---------------------------------------------------
%% completed courses' mark of current students
cstu_get_mark(jack,maths,85).
cstu_get_mark(jack,physics,76).

%% completed courses' mark of previous students
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


%% future courses which has been token by previous students.
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

%% future courses which has been passed by previous students.
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

%% major and courses groups
%% major(network_engineering,[java,operating_system],[database]).
%% major(software_engineering,[java,operating_syestem],[c_plus_plus]).

%% ---------------------------------------------------
%% FACTS that optimize the KBMC
%% ---------------------------------------------------
%% current student
%% cstu(jack).

%% previous student
%% pstu(mike).
%% pstu(hanks).
%% pstu(tom).
%% pstu(robbie).

%% current courses
%% cc(maths).
%% cc(physics).

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
%% KEY RULES to KBMC
%% ---------------------------------------------------
/*Finding out the previous students PSTU who have at least one 
similarmark of a completed course with current student CSTU.*/
similar_pstu(CSTU,PSTU):-
	cstu_get_mark(CSTU,C,M1),
	pstu_get_mark(PSTU,C,M2),
	M2=<M1+1,M2>=M1-1.
	
/*Finding out future courses TC that these privious students 
PSTU have been token similar to the current student CSTU.*/
similar_pstu_take_course(CSTU,PSTU,TC):-
	setof(PSTU,similar_pstu(CSTU,PSTU),PSTUL),
	member(PSTU,PSTUL),
	pstu_take_course(PSTU,TC).

/*Finding out future courses PC that these privious students 
PSTU have been passed similar to the current student CSTU.*/
similar_pstu_pass_course(CSTU,PSTU,PC):-
	setof(PSTU,similar_pstu(CSTU,PSTU),PSTUL),
	member(PSTU,PSTUL),
	pstu_pass_course(PSTU,PC).

/*Calculating the supporing degree of the future courses FC.*/
cal_tfc(CSTU,FC,TFC):-
	fc(FC),
	count(similar_pstu_pass_course(CSTU,_,FC),PTotal),
	count(similar_pstu_take_course(CSTU,_,FC),TTotal),
	TFC=PTotal/TTotal.
	

%% ---------------------------------------------------
%% AUXILIARY PREDICATE
%% ---------------------------------------------------
/*Counting the result of the predicate P.*/
count(P,Count) :-
	findall(1,P,L),
	length(L,Count).
%% choose_major(A,M):-major(M,C1,C2),pass(A,FC1),member(FC1,C1),pass(A,FC2),member(FC2,C2).