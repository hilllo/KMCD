%% ---------------------------------------------------
%% KEY FACTS to KBMC
%% ---------------------------------------------------
%% completed courses' mark of current students
cstu_get_mark(jack,maths,85).
cstu_get_mark(jack,physics,76).

%% completed courses' mark of previous students
%% mike
fstu_get_mark(mike,maths,80).
fstu_get_mark(mike,physics,75).
%% hanks
fstu_get_mark(hanks,maths,86).
fstu_get_mark(hanks,physics,72).
%% tom
fstu_get_mark(tom,maths,88).
fstu_get_mark(tom,physics,75).
%% robbie
fstu_get_mark(robbie,maths,85).
fstu_get_mark(robbie,physics,75).


%% future courses which has been token by previous students.
%% mike
fstu_take_course(mike,os).
fstu_take_course(mike,cpp).
fstu_take_course(mike,java).
fstu_take_course(mike,ai).
fstu_take_course(mike,ca).
fstu_take_course(mike,ed).
%% hanks
fstu_take_course(hanks,os).
fstu_take_course(hanks,java).
fstu_take_course(hanks,unix).
fstu_take_course(hanks,ai).
fstu_take_course(hanks,ca).
fstu_take_course(hanks,ed).
%% tom
fstu_take_course(tom,me).
fstu_take_course(tom,cpp).
fstu_take_course(tom,ed).
%% robbie
fstu_take_course(robbie,me).
fstu_take_course(robbie,os).
fstu_take_course(robbie,cpp).
fstu_take_course(robbie,ca).

%% future courses which has been passed by previous students.
%% mike
fstu_pass_course(mike,os).
fstu_pass_course(mike,java).
fstu_pass_course(mike,ai).
fstu_pass_course(mike,ca).
%% hanks
fstu_pass_course(hanks,os).
fstu_pass_course(hanks,java).
fstu_pass_course(hanks,unix).
fstu_pass_course(hanks,ed).
%% tom
fstu_pass_course(tom,me).
fstu_pass_course(tom,cpp).
fstu_pass_course(tom,ed).
%% robbie
fstu_pass_course(robbie,me).
fstu_pass_course(robbie,cpp).
fstu_pass_course(robbie,ca).

%% elective courses groups
%% elective_courses_group(group number,[list of elective courses]).
elective_courses_group(1,[java,cpp]).
elective_courses_group(2,[ai,unix]).
elective_courses_group(3,[ed,ca]).
elective_courses_group(4,[os,cpp]).

%% major
/* major(major name,
		[list of required courses],
		[list of elective courses groups number]).*/
major(cs,[os],[1,2,3]).
major(ee,[me],[3,4]).

%% ---------------------------------------------------
%% FACTS that optimize the KBMC
%% ---------------------------------------------------
%% current student
%% cstu(jack).

%% previous student
%% fstu(mike).
%% fstu(hanks).
%% fstu(tom).
%% fstu(robbie).

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

%% major
m(cs).
m(ee).

%% ---------------------------------------------------
%% KEY RULES to KBMC
%% ---------------------------------------------------




%% -----------
%% Tfc
%% -----------
/*Find out former students FStu who have similar capabilities
 with the current student CStu.*/
similar_fstu(CStu,FStu):-
	cstu_get_mark(CStu,C,M1),
	fstu_get_mark(FStu,C,M2),
	M2=<M1+1,M2>=M1-1.
	
/*Find out future courses TC that those former students FStu 
have been token.*/
similar_fstu_take_course(CStu,FStu,FC):-
	setof(FStu,similar_fstu(CStu,FStu),FStuL),
	member(FStu,FStuL),
	fstu_take_course(FStu,FC).

/*Find out future courses FC that those former students FStu 
have been passed.*/
similar_fstu_pass_course(CStu,FStu,FC):-
	setof(FStu,similar_fstu(CStu,FStu),FStuL),
	member(FStu,FStuL),
	fstu_pass_course(FStu,FC).

/*Count the number BhfcT of those former student who have been 
taken course FC as a support for the current student CStu which 
are brought by both the hypothesis and the evidence.*/
bhfc_take(CStu,FC,BhfcT):-
	fc(FC),
	count(similar_fstu_take_course(CStu,_,FC),BhfcT).

/*Count the number BhfcP of those former student who have been 
passed course FC as a support for the current student CStu which 
are brought by both the hypothesis and the evidence.*/
bhfc_pass(CStu,FC,BhfcP):-
	fc(FC),
	count(similar_fstu_pass_course(CStu,_,FC),BhfcP).

/*Calculate the supporting degree Tfc of the future courses FC.*/
tfc(CStu,FC,Tfc):-
	fc(FC),
	bhfc_take(CStu,FC,BhfcT),
	bhfc_pass(CStu,FC,BhfcP),
	Tfc = BhfcP/BhfcT.

/*Calculate the supporting degree of courses in a gourp ECGNum.*/
tfc_for_group(CStu,ECGNum,Tfc):-
	elective_courses_group(ECGNum,GList),
	member(FC,GList),
	tfc(CStu,FC,Tfc).

/*Calculate the max supporting degree of courses in a gourp ECGNum.*/
tfc_max(CStu,ECGNum,TfcM):-
	setof(Tfc,tfc_for_group(CStu,ECGNum,Tfc),TfcL),
	%% writef('%w',[TfcL]),
	max(TfcL,TfcM).





%% -----------
%% Bhm for elective courses 
%% -----------
/*Select a course whose supporting degree is the max in its gourp.*/
cstu_select_elective_course(CStu,ECGNum,SC,Tfc):-
	tfc_max(CStu,ECGNum,TfcM),
	elective_courses_group(ECGNum,FCL),
	member(FC,FCL),
	tfc(CStu,FC,Tfc),
	Tfc==TfcM,
	SC=FC.

/*Make a list*/
cstu_select_elective_course_list(CStu,ECGNum,SCL,Tfc):-
	setof(SC,cstu_select_elective_course(CStu,ECGNum,SC,Tfc),SCL).

%% BhmTAKE
/*Give elective courses taking status information of those former 
students who have similar capabilities with the current student*/
bhfc_elective_take_of_major(CStu,M,BhfcET):-
	major(M,_,ECGNumL),
	member(ECGNum,ECGNumL),
	cstu_select_elective_course_list(CStu,ECGNum,SCL,_),
	[SC|_]=SCL,		% When there are more than 1 course whose Tfc is the maximum of its group, only calculate 1 courses.
	bhfc_take(CStu,SC,BhfcET).
bhfc_elective_take_of_major(CStu,M,SC,BhfcET):-
	major(M,_,ECGNumL),
	member(ECGNum,ECGNumL),
	cstu_select_elective_course_list(CStu,ECGNum,SCL,_),
	[SC|_]=SCL,
	bhfc_take(CStu,SC,BhfcET).

bhm_elective_take(CStu,M,BhmET):-
	m(M),
	findall(BhfcT,bhfc_elective_take_of_major(CStu,M,BhfcT),BhfcTL),
	sum_list(BhfcTL,BhmET).

%% BhmPASS
/*Give elective courses passing status information of those former 
students who have similar capabilities with the current student*/
bhfc_elective_pass_of_major(CStu,M,BhfcEP):-
	major(M,_,ECGNumL),
	member(ECGNum,ECGNumL),
	cstu_select_elective_course_list(CStu,ECGNum,SCL,_),
	[SC|_]=SCL,
	bhfc_pass(CStu,SC,BhfcEP).
bhfc_elective_pass_of_major(CStu,M,SC,BhfcEP):-
	major(M,_,ECGNumL),
	member(ECGNum,ECGNumL),
	cstu_select_elective_course_list(CStu,ECGNum,SCL,_),
	[SC|_]=SCL,
	bhfc_pass(CStu,SC,BhfcEP).

bhm_elective_pass(CStu,M,BhmEP):-
	m(M),
	findall(BhfcP,bhfc_elective_pass_of_major(CStu,M,BhfcP),BhfcPL),
	sum_list(BhfcPL,BhmEP).





%% -----------
%% Bhm for required courses 
%% -----------
bhm_required_take(CStu,M,BhmRT):-
	major(M,RCL,_),
	member(RC,RCL),
	findall(BhfcT,bhfc_take(CStu,RC,BhfcT),BhfcTL),
	sum_list(BhfcTL,BhmRT).

bhm_required_pass(CStu,M,BhmRP):-
	major(M,RCL,_),
	member(RC,RCL),
	findall(BhfcP,bhfc_pass(CStu,RC,BhfcP),BhfcPL),
	sum_list(BhfcPL,BhmRP).




%% -----------
%% Bhm for courses 
%% -----------
bhm_take(CStu,M,BhmT):-
	bhm_required_take(CStu,M,BhmRT),
	bhm_elective_take(CStu,M,BhmET),
	BhmT is BhmRT+BhmET.

bhm_pass(CStu,M,BhmP):-
	bhm_required_pass(CStu,M,BhmRP),
	bhm_elective_pass(CStu,M,BhmEP),
	BhmP is BhmRP+BhmEP.


%% -----------
%% Tm
%% -----------
tm(CStu,Tm):-
	bhm_take(CStu,M,BhmT),
	bhm_pass(CStu,M,BhmP),
	Tm=BhmP/BhmT.
tm(CStu,M,Tm):-
	bhm_take(CStu,M,BhmT),
	bhm_pass(CStu,M,BhmP),
	Tm=BhmP/BhmT.

tm_list(CStu,TmL):-
	findall(Tm,tm(CStu,Tm),TmL).

tm_max(CStu,TmM):-
	tm_list(CStu,TmL),
	max(TmL,TmM).

choose_major(CStu,M,Tm):-
	tm_max(CStu,TmM),
	tm(CStu,M,Tm),
	Tm==TmM.

choose_major_list(CStu,ML,Tm):-
	setof(M,choose_major(CStu,M,Tm),ML).
	

%% ---------------------------------------------------
%% AUXILIARY PREDICATE
%% ---------------------------------------------------
/*Count the result of the predicate P.*/
count(P,Count) :-
	findall(1,P,L),
	length(L,Count).
	
/*Calculate the max number of a number list.*/
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

/*Calculate the sum of a number list.*/
sum_list([],0).
sum_list([H|T],Sum) :-
	sum_list(T,Rest),
	Sum is H+Rest.
	
/*Used for debug*/
%% tfc(FC) :- forall(tfc(jack,FC,Tfc),writef('T(%w) = %w\n',[FC,Tfc])).
tfcall :- forall(tfc(jack,FC,Tfc),writef('T(%w) = %w\n',[FC,Tfc])),nl.
tfcfg :- forall(tfc_for_group(jack,ECGNum,Tfc),writef('Elective course Group%w T = %w\n',[ECGNum,Tfc])),nl.
%% tfcfgl :- forall(tfc_for_group_list(jack,ECGNum,FCL,TfcL),writef('Group%w T%w = %w\n',[ECGNum,FCL,TfcL])),nl.
tfcm :- forall(tfc_max(jack,ECGNum,TfcM),writef('Elective course Group %w max Tfc= %w\n',[ECGNum,TfcM])),nl.
csec :- forall(cstu_select_elective_course(jack,ECGNum,SC,Tfc),writef('Elective course Group%w select course %w.\n T(%w) = %w\n\n',[ECGNum,SC,SC,Tfc])),nl.
csecl :- forall(cstu_select_elective_course_list(jack,ECGNum,SCL,Tfc),writef('Elective course Group %w select course %w.\t T%w = %w\n',[ECGNum,SCL,SCL,Tfc])),nl.
bhfct :- forall(bhfc_take(jack,FC,BhfcT),writef('B(%w) = %w\n',[FC,BhfcT])),nl.
bhfcetom :- forall(bhfc_elective_take_of_major(jack,M,SC,BhfcT),writef('Major %w B1h(%w) = %w\n',[M,SC,BhfcT])),nl.
bhmet :- forall(bhm_elective_take(jack,M,BhmET),writef('Major %w B1he = %w\n',[M,BhmET])),nl.
bhfcepom :- forall(bhfc_elective_pass_of_major(jack,M,SC,BhfcEP),writef('Major %w B2h(%w) = %w\n',[M,SC,BhfcEP])),nl.
bhmep :- forall(bhm_elective_pass(jack,M,BhmEP),writef('Major %w B2he = %w\n',[M,BhmEP])),nl.
bhmrt :- forall(bhm_required_take(jack,M,BhmRT),writef('Major %w B1hr = %w\n',[M,BhmRT])),nl.
bhmrp :- forall(bhm_required_pass(jack,M,BhmRP),writef('Major %w B2hr = %w\n',[M,BhmRP])),nl.
bhmt :- forall(bhm_take(jack,M,BhmP),writef('B1h(%w) = %w\n',[M,BhmP])),nl.
bhmp :- forall(bhm_pass(jack,M,BhmP),writef('B2h(%w) = %w\n',[M,BhmP])),nl.
tm :- forall(tm(jack,M,Tm),writef('T(%w) = %w\n',[M,Tm])),nl.
tml :- forall(tm_list(jack,TmL),writef('Tm = %w\n',[TmL])),nl.
tmm :- forall(tm_max(jack,TmM),writef('TmMax = %w\n',[TmM])),nl.
cm :- forall(choose_major(jack,M,Tm),writef('Choose major %w\t T(%w) = %w\n',[M,M,Tm])),nl.
cml :- forall(choose_major_list(jack,ML,Tm),writef('Choose major %w\n T%w = %w\n',[ML,ML,Tm])),nl.

main :- tfcall,tfcm,csecl,bhmt,bhmp,tm,cml.
		%% ,nl,fail;ture.