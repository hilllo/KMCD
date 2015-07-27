open_odbc:-
%% Create a new ODBC connection to data-source DSN and return a handle to this connection in Connection.
	odbc_connect('kmcd',_,
		[ user('hilllo'),
		password('2601studio'),
		alias('kmcd'),
		open('once')
]).
	
close_odbc:-
	odbc_disconnect(kmcd).



%% ---------------------------------------------------
%% KEY FACTS to KMCD read from SQL
%% ---------------------------------------------------

current_stu(CStu):-
	odbc_current_connection(Connection,_),
	odbc_query(Connection,
		'select distinct stu.stu_name 
		from cstu_course_score, stu
		where cstu_course_score.stu_id=stu.stu_id',
		row(CStu)).
	
optional_future_course(OFC) :-
	odbc_current_connection(Connection,_),
	odbc_query(Connection,
		'select distinct course.course_name 
		from fstu_course_status, course
		where fstu_course_status.course_id=course.course_id',
		row(OFC)).

%% courses' score of current students
cstu_get_score(Stu_Name,Course_Name,Course_Score):-
	odbc_current_connection(Connection,_),
	odbc_query(Connection,
		'select stu.stu_name,course.course_name,cstu_course_score.course_score 
		from cstu_course_score, stu, course 
		where cstu_course_score.stu_id=stu.stu_id and cstu_course_score.course_id = course.course_id',
		row(Stu_Name,Course_Name,Course_Score)).

%% courses' score of former students
fstu_get_score(Stu_Name,Course_Name,Course_Score):-
	odbc_current_connection(Connection,_),
	odbc_query(Connection,
		'select stu.stu_name,course.course_name,fstu_course_score.course_score 
		from fstu_course_score, stu ,course
		where fstu_course_score.stu_id=stu.stu_id and fstu_course_score.course_id = course.course_id', 
		row(Stu_Name,Course_Name,Course_Score)).

%% future courses which has been token by previous students.
%% Pass=1 means pass, Pass=0 means fail.
fstu_take_course(Stu_Name,Course_Name,Course_Status):-
	odbc_current_connection(Connection,_),
	odbc_query(Connection,
		'select stu.stu_name,course.course_name,fstu_course_status.course_status 
		from fstu_course_status, stu ,course
		where fstu_course_status.stu_id=stu.stu_id and fstu_course_status.course_id = course.course_id', 
		row(Stu_Name,Course_Name,Course_Status)).
	
%% elective courses groups
elective_courses(Group_ID,Course_Name) :-
	odbc_current_connection(Connection,_),
	odbc_query(Connection,
		'select elective_course_group.group_id,course.course_name
		from elective_course_group,course
		where elective_course_group.course_id = course.course_id', 
		row(Group_ID,Course_Name)).
	
elective_courses_group(Group_ID,Course_Name_List) :-
	setof(Course_Name,elective_courses(Group_ID,Course_Name),Course_Name_List).
	
%% compulsory courses groups for majors
major_compulsory(Major_Name,Course_Name) :-
	odbc_current_connection(Connection,_),
	odbc_query(Connection,
		'select major_compulsory.major_name,course.course_name
		from major_compulsory,course
		where major_compulsory.course_id = course.course_id', 
		row(Major_Name,Course_Name)).
	
major_compulsory_group(Major_Name,Compulsory_Group) :-
	setof(Course_Name,major_compulsory(Major_Name,Course_Name),Compulsory_Group).

%% elective courses groups for majors
major_elective(Major_Name,Group_ID) :-
	odbc_current_connection(Connection,_),
	odbc_query(Connection,
		'select major_elective.major_name,elective_course_group.group_id
		from major_elective,elective_course_group
		where major_elective.group_id = elective_course_group.group_id', 
		row(Major_Name,Group_ID)).
	
major_elective_group(Major_Name,Elective_Group) :-
	setof(Group_ID,major_elective(Major_Name,Group_ID),Elective_Group).

%% major
/* major(major name,
		[list of required courses],
		[list of elective courses groups number]).*/
major(Major_Name,Compulsory_Group,Elective_Group):-
	major_compulsory_group(Major_Name,Compulsory_Group),
	major_elective_group(Major_Name,Elective_Group).

%% cs :-forall(
%% 	cstu_get_score(CStu,Course_Name,Course_Score),
%% 	writef('%w %w %w\n',[CStu,Course_Name,Course_Score])),nl,nl.

%% fs :- forall(
%% 	fstu_get_score(Stu_Name,Course_Name,Course_Score),
%% 	writef('%w %w %w\n',[Stu_Name,Course_Name,Course_Score])),nl,nl.


%% fst :- forall(
%% 	fstu_take_course(Stu_Name,Course_Name,Course_Status),
%% 	writef('%w %w %w\n',[Stu_Name,Course_Name,Course_Status])),nl,nl.

%% test :- cs,fs,fst.

%% ====================================

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
bhfc_take(CStu,OFC,BhfcT):-
	current_stu(CStu),
	optional_future_course(OFC),
	count(similar_fstu_take_course(CStu,_,OFC),BhfcT).

/*Count the number BhfcP of those former student who have been 
passed course FC as a support for the current student CStu which 
are brought by both the hypothesis and the evidence.*/
bhfc_pass(CStu,OFC,BhfcP):-
	current_stu(CStu),
	optional_future_course(OFC),
	count(similar_fstu_pass_course(CStu,_,OFC),BhfcP).

/*Calculate the supporting degree Tfc of the future courses FC.*/
tfc(CStu,OFC,Tfc):-
	current_stu(CStu),
	optional_future_course(OFC),
	bhfc_take(CStu,OFC,BhfcT),
	bhfc_pass(CStu,OFC,BhfcP),
	Tfc = BhfcP/BhfcT.

/*Calculate the supporting degree of courses in a gourp ECGNum.*/
tfc_for_group(CStu,ECGNum,Tfc):-
	current_stu(CStu),
	elective_courses_group(ECGNum,GList),
	member(FC,GList),
	tfc(CStu,FC,Tfc).

/*Calculate the max supporting degree of courses in a gourp ECGNum.*/
tfc_max(CStu,ECGNum,TfcM):-
	current_stu(CStu),
	setof(Tfc,tfc_for_group(CStu,ECGNum,Tfc),TfcL),
	%% writef('%w',[TfcL]),
	max(TfcL,TfcM).





%% -----------
%% Bhm for elective courses 
%% -----------
/*Select a course whose supporting degree is the max in its gourp.*/
cstu_select_elective_course(CStu,ECGNum,SC,Tfc):-
	current_stu(CStu),
	tfc_max(CStu,ECGNum,TfcM),
	elective_courses_group(ECGNum,FCL),
	member(FC,FCL),
	tfc(CStu,FC,Tfc),
	Tfc==TfcM,
	SC=FC.

/*Make a list*/
cstu_select_elective_course_list(CStu,ECGNum,SCL,Tfc):-
	current_stu(CStu),
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
	current_stu(CStu),
	major(M,_,ECGNumL),
	member(ECGNum,ECGNumL),
	cstu_select_elective_course_list(CStu,ECGNum,SCL,_),
	[SC|_]=SCL,
	bhfc_take(CStu,SC,BhfcET).

bhm_elective_take(CStu,M,BhmET):-
	current_stu(CStu),
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
	current_stu(CStu),
	major(M,_,ECGNumL),
	member(ECGNum,ECGNumL),
	cstu_select_elective_course_list(CStu,ECGNum,SCL,_),
	[SC|_]=SCL,
	bhfc_pass(CStu,SC,BhfcEP).

bhm_elective_pass(CStu,M,BhmEP):-
	current_stu(CStu),
	m(M),
	findall(BhfcP,bhfc_elective_pass_of_major(CStu,M,BhfcP),BhfcPL),
	sum_list(BhfcPL,BhmEP).





%% -----------
%% Bhm for required courses 
%% -----------
bhm_required_take(CStu,M,BhmRT):-
	current_stu(CStu),
	major(M,RCL,_),
	member(RC,RCL),
	findall(BhfcT,bhfc_take(CStu,RC,BhfcT),BhfcTL),
	sum_list(BhfcTL,BhmRT).

bhm_required_pass(CStu,M,BhmRP):-
	current_stu(CStu),
	major(M,RCL,_),
	member(RC,RCL),
	findall(BhfcP,bhfc_pass(CStu,RC,BhfcP),BhfcPL),
	sum_list(BhfcPL,BhmRP).




%% -----------
%% Bhm for courses 
%% -----------
bhm_take(CStu,M,BhmT):-
	current_stu(CStu),
	bhm_required_take(CStu,M,BhmRT),
	bhm_elective_take(CStu,M,BhmET),
	BhmT is BhmRT+BhmET.

bhm_pass(CStu,M,BhmP):-
	current_stu(CStu),
	bhm_required_pass(CStu,M,BhmRP),
	bhm_elective_pass(CStu,M,BhmEP),
	BhmP is BhmRP+BhmEP.


%% -----------
%% Tm
%% -----------
tm(CStu,Tm):-
	current_stu(CStu),
	bhm_take(CStu,M,BhmT),
	bhm_pass(CStu,M,BhmP),
	Tm=BhmP/BhmT.
tm(CStu,M,Tm):-
	bhm_take(CStu,M,BhmT),
	bhm_pass(CStu,M,BhmP),
	Tm=BhmP/BhmT.

tm_list(CStu,TmL):-
	current_stu(CStu),
	findall(Tm,tm(CStu,Tm),TmL).

tm_max(CStu,TmM):-
	current_stu(CStu),
	tm_list(CStu,TmL),
	max(TmL,TmM).

choose_major(CStu,M,Tm):-
	current_stu(CStu),
	tm_max(CStu,TmM),
	tm(CStu,M,Tm),
	Tm==TmM.

choose_major_list(CStu,ML,Tm):-
	current_stu(CStu),
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
cstu :- write('Current students who are going to choose majors:\n'),forall(current_stu(CStu),writef('%w ',[CStu])),nl,nl.
fc :- write('Optional Future Courses:\n'),forall(optional_future_course(OFC),writef('%w ',[OFC])),nl,nl.
m :- write('Optional Future Major:\n'),forall(m(M),writef('%w ',[M])),nl,nl.

tfcall :- forall(tfc(_,FC,Tfc),writef('T(%w) = %w\n',[FC,Tfc])),nl.
tfcfg :- forall(tfc_for_group(_,ECGNum,Tfc),writef('Elective course Group%w T = %w\n',[ECGNum,Tfc])),nl.
tfcm :- forall(tfc_max(_,ECGNum,TfcM),writef('Elective course Group %w max Tfc= %w\n',[ECGNum,TfcM])),nl.
csec :- forall(cstu_select_elective_course(_,ECGNum,SC,Tfc),writef('Elective course Group%w select course %w.\n T(%w) = %w\n\n',[ECGNum,SC,SC,Tfc])),nl.
csecl :- forall(cstu_select_elective_course_list(_,ECGNum,SCL,Tfc),writef('Elective course Group %w select course %w.\t T%w = %w\n',[ECGNum,SCL,SCL,Tfc])),nl.

bhfct :- forall(bhfc_take(_,FC,BhfcT),writef('bhfct B(%w) = %w\n',[FC,BhfcT])),nl.
bhfcetom :- forall(bhfc_elective_take_of_major(_,M,SC,BhfcT),writef('bhfcetom Major %w B1h(%w) = %w\n',[M,SC,BhfcT])),nl.
bhmet :- forall(bhm_elective_take(_,M,BhmET),writef('bhmet Major %w B1he = %w\n',[M,BhmET])),nl.

bhfcp :- forall(bhfc_pass(_,FC,BhfcP),writef('bhfcp B(%w) = %w\n',[FC,BhfcP])),nl.
bhfcepom :- forall(bhfc_elective_pass_of_major(_,M,SC,BhfcEP),writef('bhfcepom Major %w B2h(%w) = %w\n',[M,SC,BhfcEP])),nl.
bhmep :- forall(bhm_elective_pass(_,M,BhmEP),writef('bhmep Major %w B2he = %w\n',[M,BhmEP])),nl.

bhmrt :- forall(bhm_required_take(_,M,BhmRT),writef('bhmrt Major %w B1hr = %w\n',[M,BhmRT])),nl.
bhmrp :- forall(bhm_required_pass(_,M,BhmRP),writef('bhmrp Major %w B2hr = %w\n',[M,BhmRP])),nl.

bhmt :- forall(bhm_take(_,M,BhmP),writef('bhmt B1h(%w) = %w\n',[M,BhmP])),nl.
bhmp :- forall(bhm_pass(_,M,BhmP),writef('bhmp B2h(%w) = %w\n',[M,BhmP])),nl.

tm :- forall(tm(_,M,Tm),writef('T(%w) = %w\n',[M,Tm])),nl.
tml :- forall(tm_list(_,TmL),writef('Tm = %w\n',[TmL])),nl.
tmm :- forall(tm_max(_,TmM),writef('TmMax = %w\n',[TmM])),nl.

cm :- forall(choose_major(_,M,Tm),writef('Choose major %w\t T(%w) = %w\n',[M,M,Tm])),nl.
cml :- forall(choose_major_list(CStu,ML,Tm),writef('%w is recommended to choose major %w\n T%w = %w\n',[CStu,ML,ML,Tm])),nl.

main :- open_odbc,
	cstu,fc,m,
	csecl,
%% bhfct,
%% bhfcetom,
%% bhmet,
%% bhfcp,
%% bhfcepom,
%% bhmep,
%% bhmrt,
%% bhmrp,
bhmt,
bhmp,
	tm,
	cml,
	close_odbc.