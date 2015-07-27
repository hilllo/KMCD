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

%% courses groups
%% courses_group(group number,[list of courses]).
courses_group(1,[java,cpp]).
courses_group(2,[ai,unix]).
courses_group(3,[ed,ca]).
courses_group(4,[os,cpp]).

%% major
%% major(major name,[list of group number]).
major(os,[1,2,3]).
major(ee,[3,4]).

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
/*Find out the previous students PSTU who have at least one 
similar mark of a completed course with current student CSTU.*/
similar_pstu(CSTU,PSTU):-
	cstu_get_mark(CSTU,C,M1),
	pstu_get_mark(PSTU,C,M2),
	M2=<M1+1,M2>=M1-1.
	
/*Find out future courses TC that these privious students 
PSTU have been token similar to the current student CSTU.*/
similar_pstu_take_course(CSTU,PSTU,TC):-
	setof(PSTU,similar_pstu(CSTU,PSTU),PSTUL),
	member(PSTU,PSTUL),
	pstu_take_course(PSTU,TC).

/*Find out future courses PC that these privious students 
PSTU have been passed similar to the current student CSTU.*/
similar_pstu_pass_course(CSTU,PSTU,PC):-
	setof(PSTU,similar_pstu(CSTU,PSTU),PSTUL),
	member(PSTU,PSTUL),
	pstu_pass_course(PSTU,PC).

/*Calculate the supporting degree of the future courses FC.*/
tfc(CSTU,FC,TFC):-
	fc(FC),
	count(similar_pstu_pass_course(CSTU,_,FC),PTotal),
	count(similar_pstu_take_course(CSTU,_,FC),TTotal),
	TFC=PTotal/TTotal.
	
	
/*Calculate the supporting degree of courses in a gourp GNum.*/
tfc_for_group(CSTU,GNum,TFC):-
	courses_group(GNum,GList),
	member(FC,GList),
	tfc(CSTU,FC,TFC).
	%% max(GList,Max),

/*Calculate the max supporting degree of courses in a gourp GNum.*/
tfc_max(CSTU,GNum,TFCM):-
	setof(TFC,tfc_for_group(CSTU,GNum,TFC),TFCL),
	max(TFCL,TFCM).

/*Select a course whose supporting degree is the max in its gourp.*/
cstu_select_course(CSTU,GNum,SFC,TFC):-
	tfc_max(CSTU,GNum,TFCM),
	courses_group(GNum,FCL),
	member(FC,FCL),
	tfc(CSTU,FC,TFC),
	TFC==TFCM,
	SFC=FC.


%% ---------------------------------------------------
%% AUXILIARY PREDICATE
%% ---------------------------------------------------
/*Counting the result of the predicate P.*/
count(P,Count) :-
	findall(1,P,L),
	length(L,Count).
	
/*Calculating the max number of a list*/
accMax([H|T],A,Max):-
	H>A,
	accMax(T,H,Max).
accMax([H|T],A,Max):-
	H=<A,
	accMax(T,A,Max).
accMax([],A,A).
max(List,Max):-
	List=[H|_],
	accMax(List,H,Max).
	
/*Used for debug*/
%% tfc(FC) :- forall(tfc(jack,FC,TFC),writef('T(%w) = %w\n',[FC,TFC])).
tfcall :- forall(tfc(jack,FC,TFC),writef('T(%w) = %w\n',[FC,TFC])).
tfcfg :- forall(tfc_for_group(jack,GNum,TFC),writef('Group%w = %w\n',[GNum,TFC])).
tfcm :- forall(tfc_max(jack,GNum,TFCM),writef('Group%w max TFC= %w\n',[GNum,TFCM])).
sc :- forall(cstu_select_course(jack,GNum,SFC,TFC),writef('Group%w select course %w.\nT(%w) = TFC\n\n',[GNum,SFC,TFC])).
main :- sc.
		%% ,nl,fail;ture.
%% choose_major(A,M):-major(M,C1,C2),pass(A,FC1),member(FC1,C1),pass(A,FC2),member(FC2,C2).