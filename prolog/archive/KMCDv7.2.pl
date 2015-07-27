
current_stu('Jack').

%% elective courses groups
%% elective_courses_group(Group_ID,[list of elective courses]).
%% elective_courses_group(1,['Java','Cpp']).
%% elective_courses_group(2,['AI','Unix']).
%% elective_courses_group(3,['ED','CA']).
%% elective_courses_group(4,['OS','Cpp']).



%% major
/* major(major name,
		[list of required courses],
		[list of elective courses groups number]).*/
major('CS',['OS'],[1,2,3]).
major('EE',['ME'],[3,4]).


%%=======================================
open_odbc:-
%% Create a new ODBC connection to data-source DSN and return a handle to this connection in Connection.
	odbc_connect('kmcd',_,
		[ user('hilllo'),
		password('2601studio'),
		alias('kmcd'),
		open('once')
]).

cstu_get_score(Stu_Name,Course_Name,Course_Score):-
	odbc_current_connection(Connection,_),
	odbc_query(Connection,
		'select stu.stu_name,course.course_name,cstu_course_score.course_score 
		from cstu_course_score, stu, course 
		where cstu_course_score.stu_id=stu.stu_id and cstu_course_score.course_id = course.course_id',
		row(Stu_Name,Course_Name,Course_Score)).

fstu_get_score(Stu_Name,Course_Name,Course_Score):-
	odbc_current_connection(Connection,_),
	odbc_query(Connection,
		'select stu.stu_name,course.course_name,fstu_course_score.course_score 
		from fstu_course_score, stu ,course
		where fstu_course_score.stu_id=stu.stu_id and fstu_course_score.course_id = course.course_id', 
		row(Stu_Name,Course_Name,Course_Score)).

fstu_take_course(Stu_Name,Course_Name,Course_Status):-
	odbc_current_connection(Connection,_),
	odbc_query(Connection,
		'select stu.stu_name,course.course_name,fstu_course_status.course_status 
		from fstu_course_status, stu ,course
		where fstu_course_status.stu_id=stu.stu_id and fstu_course_status.course_id = course.course_id', 
		row(Stu_Name,Course_Name,Course_Status)).
	
elective_courses(Group_ID,Course_Name) :-
	odbc_current_connection(Connection,_),
	odbc_query(Connection,
		'select elective_course_group.group_id,course.course_name
		from elective_course_group,course
		where elective_course_group.course_id = course.course_id', 
		row(Group_ID,Course_Name)).
	
elective_courses_group(Group_ID,CL) :-
	setof(C,elective_courses(Group_ID,C),CL).

%% cs :-forall(
%% 	cstu_get_score('Jack',Course_Name,Course_Score),
%% 	writef('%w %w %w\n',['Jack',Course_Name,Course_Score])),nl,nl.

%% fs :- forall(
%% 	fstu_get_score(Stu_Name,Course_Name,Course_Score),
%% 	writef('%w %w %w\n',[Stu_Name,Course_Name,Course_Score])),nl,nl.


%% fst :- forall(
%% 	fstu_take_course(Stu_Name,Course_Name,Course_Status),
%% 	writef('%w %w %w\n',[Stu_Name,Course_Name,Course_Status])),nl,nl.

%% test :- cs,fs,fst.

%% ====================================
fc_get(FC) :- setof(FC,fstu_take_course(_,FC,_),FCPStuL),member(FC,FCPStuL).
fc_list(FCL) :- setof(FC,fc_get(FC),FCL).
fc(FC) :- fc_list(FCL),member(FC,FCL).

%% major
m(M) :- major(M,_,_).
%% m(cs).
%% m(ee).

%% ---------------------------------------------------
%% KEY RULES to KBMC
%% ---------------------------------------------------




%% -----------
%% Tfc
%% -----------
/*Find out former students FStu who have similar capabilities
 with the current student CStu.*/
similar_fstu(CStu,FStu):-
	cstu_get_score(CStu,C,Score1),
	fstu_get_score(FStu,C,Score2),
	Score2=<Score1+1,Score2>=Score1-1.
	
/*Find out future courses TC that those former students FStu 
have been token.*/
similar_fstu_take_course(CStu,FStu,FC):-
	setof(FStu,similar_fstu(CStu,FStu),FStuL),
	member(FStu,FStuL),
	fstu_take_course(FStu,FC,_).

/*Find out future courses FC that those former students FStu 
have been passed.*/
similar_fstu_pass_course(CStu,FStu,FC):-
	setof(FStu,similar_fstu(CStu,FStu),FStuL),
	member(FStu,FStuL),
	fstu_take_course(FStu,FC,1).

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
fc :- write('Now we have these courses:\n'),forall(fc(FC),writef('%w ',[FC])),nl,nl.
m :- write('Now we have these major:\n'),forall(m(M),writef('%w ',[M])),nl,nl.

tfcall :- forall(tfc('Jack',FC,Tfc),writef('T(%w) = %w\n',[FC,Tfc])),nl.
tfcfg :- forall(tfc_for_group('Jack',ECGNum,Tfc),writef('Elective course Group%w T = %w\n',[ECGNum,Tfc])),nl.
tfcm :- forall(tfc_max('Jack',ECGNum,TfcM),writef('Elective course Group %w max Tfc= %w\n',[ECGNum,TfcM])),nl.
csec :- forall(cstu_select_elective_course('Jack',ECGNum,SC,Tfc),writef('Elective course Group%w select course %w.\n T(%w) = %w\n\n',[ECGNum,SC,SC,Tfc])),nl.
csecl :- forall(cstu_select_elective_course_list('Jack',ECGNum,SCL,Tfc),writef('Elective course Group %w select course %w.\t T%w = %w\n',[ECGNum,SCL,SCL,Tfc])),nl.

bhfct :- forall(bhfc_take('Jack',FC,BhfcT),writef('B(%w) = %w\n',[FC,BhfcT])),nl.
bhfcetom :- forall(bhfc_elective_take_of_major('Jack',M,SC,BhfcT),writef('Major %w B1h(%w) = %w\n',[M,SC,BhfcT])),nl.
bhmet :- forall(bhm_elective_take('Jack',M,BhmET),writef('Major %w B1he = %w\n',[M,BhmET])),nl.

bhfcp :- forall(bhfc_pass('Jack',FC,BhfcP),writef('B(%w) = %w\n',[FC,BhfcP])),nl.
bhfcepom :- forall(bhfc_elective_pass_of_major('Jack',M,SC,BhfcEP),writef('Major %w B2h(%w) = %w\n',[M,SC,BhfcEP])),nl.
bhmep :- forall(bhm_elective_pass('Jack',M,BhmEP),writef('Major %w B2he = %w\n',[M,BhmEP])),nl.

bhmrt :- forall(bhm_required_take('Jack',M,BhmRT),writef('Major %w B1hr = %w\n',[M,BhmRT])),nl.
bhmrp :- forall(bhm_required_pass('Jack',M,BhmRP),writef('Major %w B2hr = %w\n',[M,BhmRP])),nl.

bhmt :- forall(bhm_take('Jack',M,BhmP),writef('B1h(%w) = %w\n',[M,BhmP])),nl.
bhmp :- forall(bhm_pass('Jack',M,BhmP),writef('B2h(%w) = %w\n',[M,BhmP])),nl.

tm :- forall(tm('Jack',M,Tm),writef('T(%w) = %w\n',[M,Tm])),nl.
tml :- forall(tm_list('Jack',TmL),writef('Tm = %w\n',[TmL])),nl.
tmm :- forall(tm_max('Jack',TmM),writef('TmMax = %w\n',[TmM])),nl.

cm :- forall(choose_major('Jack',M,Tm),writef('Choose major %w\t T(%w) = %w\n',[M,M,Tm])),nl.
cml :- forall(choose_major_list('Jack',ML,Tm),writef('Choose major %w\n T%w = %w\n',[ML,ML,Tm])),nl.

main :- open_odbc,fc,m,csecl,bhmt,bhmp,tm,cml.