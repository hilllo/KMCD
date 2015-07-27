open_odbc:-
%% Create a new ODBC connection to data-source DSN and return a handle to this connection in Connection.
	odbc_connect('kmcd',_,
		[ user('hilllo'),
		password('2601studio'),
		alias('kmcd'),
		open('once')]
	).
	
close_odbc:-
	odbc_disconnect(kmcd).



%% ---------------------------------------------------
%% KEY FACTS to KMCD read from SQL
%% ---------------------------------------------------

current_stu(CSTU_ID,CSTU_NAME):-
	odbc_current_connection(Connection,_),
	odbc_query(Connection,'
		select distinct stu_id,stu_name 
		from stu
		where stu.stu_prospect="unknown"',
		row(CSTU_ID,CSTU_NAME)
		).

former_stu(FSTU_ID,FSTU_NAME):-
	odbc_current_connection(Connection,_),
	odbc_query(Connection,'
		select distinct stu_id,stu_name 
		from stu
		where stu.stu_prospect="graduated"',
		row(FSTU_ID,FSTU_NAME)
		).

course(COURSE_ID,COURSE_NAME) :-
	odbc_current_connection(Connection,_),
	odbc_query(Connection,'
		select distinct course.course_id, course.course_name
		from course',
		row(COURSE_ID,COURSE_NAME)
		).

major(MAJOR_ID,MAJOR_NAME) :-
	odbc_current_connection(Connection,_),
	odbc_query(Connection,'
		select distinct major.major_id, major.major_name
		from major',
		row(MAJOR_ID,MAJOR_NAME)
		).

cstu_course_id(CSTU_COURSE_ID) :-
	odbc_current_connection(Connection,_),
	odbc_query(Connection,'
		select distinct stu_scoreofcourse.course_id
		from stu,stu_scoreofcourse
		where stu.stu_id=stu_scoreofcourse.stu_id 
		and stu.stu_prospect="unknown"',
		row(CSTU_COURSE_ID)
		).

optional_future_course_id(OPTIONAL_COURSE_ID) :-
	setof(CSTU_COURSE_ID,cstu_course_id(CSTU_COURSE_ID),CSTU_COURSE_ID_LIST),
	setof(ALL_COURSE_ID,course(ALL_COURSE_ID,_),ALL_COURSE_ID_LIST),
	subtract(ALL_COURSE_ID_LIST,CSTU_COURSE_ID_LIST,OPTIONAL_COURSE_ID_LIST),
	member(OPTIONAL_COURSE_ID,OPTIONAL_COURSE_ID_LIST).

optional_future_course(OPTIONAL_COURSE_ID,OPTIONAL_COURSE_NAME) :-
	optional_future_course_id(OPTIONAL_COURSE_ID),
	course(OPTIONAL_COURSE_ID,OPTIONAL_COURSE_NAME).

get_score(STU_ID,COURSE_ID,COURSE_SCORE) :-
	odbc_current_connection(Connection,_),
	odbc_query(Connection,'
		select stu.stu_id,course.course_id,stu_scoreofcourse.course_score
		from stu,course,stu_scoreofcourse
		where stu.stu_id=stu_scoreofcourse.stu_id
		and course.course_id=stu_scoreofcourse.course_id',
		row(STU_ID,COURSE_ID,COURSE_SCORE)
		).

%% courses' score of current students
cstu_get_score(CSTU_ID,CSTU_COURSE_ID,CSTU_COURSE_SCORE) :-
	current_stu(CSTU_ID,_),
	get_score(CSTU_ID,CSTU_COURSE_ID,CSTU_COURSE_SCORE).

%% courses' score of former students
fstu_get_score(FSTU_ID,CSTU_COURSE_ID,FSTU_COURSE_SCORE) :-
	former_stu(FSTU_ID,_),
	cstu_course_id(CSTU_COURSE_ID),
	get_score(FSTU_ID,CSTU_COURSE_ID,FSTU_COURSE_SCORE).

%% future courses which has been token by previous students.
%% fstu_take_course(FSTU_ID,OPTIONAL_COURSE_ID,COURSE_STATUS)
%% COURSE_STATUS=1 means pass, COURSE_STATUS=0 means fail.
fstu_take_course(FSTU_ID,OPTIONAL_COURSE_ID,1):- %%PASS
	former_stu(FSTU_ID,_),
	optional_future_course_id(OPTIONAL_COURSE_ID),
	get_score(FSTU_ID,OPTIONAL_COURSE_ID,COURSE_SCORE ),
	COURSE_SCORE >= 60, COURSE_SCORE =< 100.
	
fstu_take_course(FSTU_ID,OPTIONAL_COURSE_ID,0):- %%FAIL
	former_stu(FSTU_ID,_),
	optional_future_course_id(OPTIONAL_COURSE_ID),
	get_score(FSTU_ID,OPTIONAL_COURSE_ID,COURSE_SCORE ),
	COURSE_SCORE >= 0, COURSE_SCORE < 60.

%% elective courses groups
courses_group_id(GROUP_ID,COURSE_ID,R_E) :-
	odbc_current_connection(Connection,_),
	odbc_query(Connection,'
	select course_group.group_id,course_group.course_id,major_courses.r_e
	from course_group,major_courses
	where course_group.group_id=major_courses.group_id',
	row(GROUP_ID,COURSE_ID,R_E)
	).

elective_courses_group_id_list(EGROUP_ID,ECOURSE_ID_LIST) :-
	setof(ECOURSE_ID,courses_group_id(EGROUP_ID,ECOURSE_ID,'E'),ECOURSE_ID_LIST).

major_group_id(MAJOR_ID,GROUP_ID,R_E) :-
	odbc_current_connection(Connection,_),
	odbc_query(Connection,'
	select major_id,group_id,r_e
	from major_courses',
	row(MAJOR_ID,GROUP_ID,R_E)
	).
	
major_group_id_list(MAJOR_ID,GROUP_ID_LIST,R_E) :-
	setof(GROUP_ID,major_group_id(MAJOR_ID,GROUP_ID,R_E),GROUP_ID_LIST).

required_courses_id(MAJOR_ID,RCOURSE_ID) :-
	major_group_id_list(MAJOR_ID,RGROUP_ID_LIST,'R'),
	member(RGROUP_ID,RGROUP_ID_LIST),
	courses_group_id(RGROUP_ID,RCOURSE_ID,_).

major_rcourse_id(MAJOR_ID,RCOURSE_ID_LIST) :-
	setof(RCOURSE_ID,required_courses_id(MAJOR_ID,RCOURSE_ID),RCOURSE_ID_LIST).

major_regroup_id_list(MAJOR_ID,RCOURSE_ID_LIST,EGROUP_ID_LIST):-
	major_rcourse_id(MAJOR_ID,RCOURSE_ID_LIST),
	major_group_id_list(MAJOR_ID,EGROUP_ID_LIST,'E').



%% ---------------------------------------------------
%% KEY RULES to KBMC
%% ---------------------------------------------------




%% -----------
%% Tfc
%% -----------
/*Find out former students FStu who have similar capabilities
 with the current student CStu.*/
similar_fstu(CSTU_ID,FSTU_ID):-
	cstu_course_id(CSTU_COURSE_ID),
	cstu_get_score(CSTU_ID,CSTU_COURSE_ID,CSTU_COURSE_SCORE),
	fstu_get_score(FSTU_ID,CSTU_COURSE_ID,FSTU_COURSE_SCORE),
	FSTU_COURSE_SCORE=<CSTU_COURSE_SCORE+1,FSTU_COURSE_SCORE>=CSTU_COURSE_SCORE-1.
	
/*Find out future courses TC that those former students FStu 
have been token.*/
similar_fstu_take_course(CSTU_ID,FSTU_ID,FSTU_COURSE_ID):-
	setof(FSTU_ID,similar_fstu(CSTU_ID,FSTU_ID),FSTU_ID_LIST),
	member(FSTU_ID,FSTU_ID_LIST),
	fstu_take_course(FSTU_ID,FSTU_COURSE_ID,_).

/*Find out future courses FC that those former students FStu 
have been passed.*/
similar_fstu_pass_course(CStu,FStu,FC):-
	setof(FStu,similar_fstu(CStu,FStu),FStuL),
	member(FStu,FStuL),
	fstu_take_course(FStu,FC,1).

/*Count the number BhfcT of those former student who have been 
taken course FC as a support for the current student CStu which 
are brought by both the hypothesis and the evidence.*/
bhfc_take(CSTU_ID,OFC,BhfcT):-
	current_stu(CSTU_ID,_),
	optional_future_course_id(OFC),
	count(similar_fstu_take_course(CSTU_ID,_,OFC),BhfcT).

/*Count the number BhfcP of those former student who have been 
passed course FC as a support for the current student CStu which 
are brought by both the hypothesis and the evidence.*/
bhfc_pass(CSTU_ID,OFC,BhfcP):-
	current_stu(CSTU_ID,_),
	optional_future_course_id(OFC),
	count(similar_fstu_pass_course(CSTU_ID,_,OFC),BhfcP).

/*Calculate the supporting degree Tfc of the future courses FC.*/
tfc(CSTU_ID,OFC,Tfc):-
	current_stu(CSTU_ID,_),
	optional_future_course_id(OFC),
	bhfc_take(CSTU_ID,OFC,BhfcT),
	bhfc_pass(CSTU_ID,OFC,BhfcP),
	Tfc = BhfcP/BhfcT.

/*Calculate the supporting degree of courses in a gourp ECGNum.*/
tfc_for_group(CSTU_ID,ECGNum,Tfc):-
	current_stu(CSTU_ID,_),
	elective_courses_group_id_list(ECGNum,GList),
	member(FC,GList),
	tfc(CSTU_ID,FC,Tfc).

/*Calculate the max supporting degree of courses in a gourp ECGNum.*/
tfc_max(CSTU_ID,ECGNum,TfcM):-
	current_stu(CSTU_ID,_),
	setof(Tfc,tfc_for_group(CSTU_ID,ECGNum,Tfc),TfcL),
	%% writef('%w',[TfcL]),
	max(TfcL,TfcM).





%% -----------
%% Bhm for elective courses 
%% -----------
/*Select a course whose supporting degree is the max in its gourp.*/
cstu_select_elective_course(CSTU_ID,ECGNum,SC,Tfc):-
	current_stu(CSTU_ID,_),
	tfc_max(CSTU_ID,ECGNum,TfcM),
	elective_courses_group_id_list(ECGNum,FCL),
	member(FC,FCL),
	tfc(CSTU_ID,FC,Tfc),
	Tfc==TfcM,
	SC=FC.

/*Make a list*/
cstu_select_elective_course_list(CSTU_ID,ECGNum,SCL,Tfc):-
	current_stu(CSTU_ID,_),
	setof(SC,cstu_select_elective_course(CSTU_ID,ECGNum,SC,Tfc),SCL).

%% BhmTAKE
/*Give elective courses taking status information of those former 
students who have similar capabilities with the current student*/
bhfc_elective_take_of_major(CStu,M,BhfcET):-
	major_regroup_id_list(M,_,ECGNumL),
	member(ECGNum,ECGNumL),
	cstu_select_elective_course_list(CStu,ECGNum,SCL,_),
	[SC|_]=SCL,		% When there are more than 1 course whose Tfc is the maximum of its group, only calculate 1 courses.
	bhfc_take(CStu,SC,BhfcET).
bhfc_elective_take_of_major(CSTU_ID,M,SC,BhfcET):-
	current_stu(CSTU_ID,_),
	major_regroup_id_list(M,_,ECGNumL),
	member(ECGNum,ECGNumL),
	cstu_select_elective_course_list(CSTU_ID,ECGNum,SCL,_),
	[SC|_]=SCL,
	bhfc_take(CSTU_ID,SC,BhfcET).

bhm_elective_take(CSTU_ID,MAJOR_ID,BhmET):-
	current_stu(CSTU_ID,_),
	major(MAJOR_ID,_),
	findall(BhfcT,bhfc_elective_take_of_major(CSTU_ID,MAJOR_ID,BhfcT),BhfcTL),
	sum_list(BhfcTL,BhmET).

%% BhmPASS
/*Give elective courses passing status information of those former 
students who have similar capabilities with the current student*/
bhfc_elective_pass_of_major(CSTU_ID,M,BhfcEP):-
	major_regroup_id_list(M,_,ECGNumL),
	member(ECGNum,ECGNumL),
	cstu_select_elective_course_list(CSTU_ID,ECGNum,SCL,_),
	[SC|_]=SCL,
	bhfc_pass(CSTU_ID,SC,BhfcEP).
bhfc_elective_pass_of_major(CSTU_ID,M,SC,BhfcEP):-
	current_stu(CSTU_ID,_),
	major_regroup_id_list(M,_,ECGNumL),
	member(ECGNum,ECGNumL),
	cstu_select_elective_course_list(CSTU_ID,ECGNum,SCL,_),
	[SC|_]=SCL,
	bhfc_pass(CSTU_ID,SC,BhfcEP).

bhm_elective_pass(CSTU_ID,MAJOR_ID,BhmEP):-
	current_stu(CSTU_ID,_),
	major(MAJOR_ID,_),
	findall(BhfcP,bhfc_elective_pass_of_major(CSTU_ID,MAJOR_ID,BhfcP),BhfcPL),
	sum_list(BhfcPL,BhmEP).





%% -----------
%% Bhm for required courses 
%% -----------
bhm_required_take(CSTU_ID,M,BhmRT):-
	current_stu(CSTU_ID,_),
	major_regroup_id_list(M,RCL,_),
	member(RC,RCL),
	findall(BhfcT,bhfc_take(CSTU_ID,RC,BhfcT),BhfcTL),
	sum_list(BhfcTL,BhmRT).

bhm_required_pass(CSTU_ID,M,BhmRP):-
	current_stu(CSTU_ID,_),
	major_regroup_id_list(M,RCL,_),
	member(RC,RCL),
	findall(BhfcP,bhfc_pass(CSTU_ID,RC,BhfcP),BhfcPL),
	sum_list(BhfcPL,BhmRP).




%% -----------
%% Bhm for courses 
%% -----------
bhm_take(CSTU_ID,M,BhmT):-
	current_stu(CSTU_ID,_),
	bhm_required_take(CSTU_ID,M,BhmRT),
	bhm_elective_take(CSTU_ID,M,BhmET),
	BhmT is BhmRT+BhmET.

bhm_pass(CSTU_ID,M,BhmP):-
	current_stu(CSTU_ID,_),
	bhm_required_pass(CSTU_ID,M,BhmRP),
	bhm_elective_pass(CSTU_ID,M,BhmEP),
	BhmP is BhmRP+BhmEP.


%% -----------
%% Tm
%% -----------
tm(CSTU_ID,Tm):-
	current_stu(CSTU_ID,_),
	bhm_take(CSTU_ID,M,BhmT),
	bhm_pass(CSTU_ID,M,BhmP),
	Tm=BhmP/BhmT.
tm(CSTU_ID,M,Tm):-
	bhm_take(CSTU_ID,M,BhmT),
	bhm_pass(CSTU_ID,M,BhmP),
	Tm=BhmP/BhmT.

tm_list(CSTU_ID,TmL):-
	current_stu(CSTU_ID,_),
	findall(Tm,tm(CSTU_ID,Tm),TmL).

tm_max(CSTU_ID,TmM):-
	current_stu(CSTU_ID,_),
	tm_list(CSTU_ID,TmL),
	max(TmL,TmM).

choose_major(CSTU_ID,CSTU_NAME,M,MAJOR_NAME,Tm):-
	current_stu(CSTU_ID,CSTU_NAME),
	tm_max(CSTU_ID,TmM),
	tm(CSTU_ID,M,Tm),
	Tm==TmM,
	major(M,MAJOR_NAME).

%% choose_major_list(CSTU_ID,ML,MAJOR_NAME,Tm):-
%% 	current_stu(CSTU_ID,_),
%% 	setof(M,choose_major(CSTU_ID,M,MAJOR_NAME,Tm),ML).
	

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
cstu :- write('Current students who are going to choose majors:\n'),forall(current_stu(CSTU_ID,CSTU_NAME),writef('[%w]%w ',[CSTU_ID,CSTU_NAME])),nl,nl.
fstu :- write('Former graduated students:\n'),forall(former_stu(FSTU_ID,FSTU_NAME),writef('[%w]%w\n',[FSTU_ID,FSTU_NAME])),nl,nl.
fcid :- write('Optional Future Courses ID:\n'),forall(optional_future_course_id(OPTIONAL_COURSE_ID),writef('[%w] \n',[OPTIONAL_COURSE_ID])),nl,nl.
fc:- write('Optional Future Courses:\n'),forall(optional_future_course(OPTIONAL_COURSE_ID,OPTIONAL_COURSE_NAME),writef('[%w]%w\n',[OPTIONAL_COURSE_ID,OPTIONAL_COURSE_NAME])),nl,nl.
m :- write('Optional Future Major:\n'),forall(major(MAJOR_ID,MAJOR_NAME),writef('[%w]%w\n',[MAJOR_ID,MAJOR_NAME])),nl,nl.

cstu_gs :- forall(cstu_get_score(CSTU_ID,CSTU_COURSE_NAME,CSTU_COURSE_SCORE),writef('[%w]%w %w \n',[CSTU_ID,CSTU_COURSE_NAME,CSTU_COURSE_SCORE])),nl,nl.
fstu_gs :- forall(fstu_get_score(FSTU_ID,FSTU_COURSE_NAME,FSTU_COURSE_SCORE),writef('[%w]%w %w \n',[FSTU_ID,FSTU_COURSE_NAME,FSTU_COURSE_SCORE])),nl,nl.
fst_tc_pass :-forall(fstu_take_course(FSTU_ID,PTIONAL_COURSE_ID,1),writef('[%w]%w\n',[FSTU_ID,PTIONAL_COURSE_ID])),nl,nl.
fst_tc_fail :-forall(fstu_take_course(FSTU_ID,PTIONAL_COURSE_ID,0),writef('[%w]%w\n',[FSTU_ID,PTIONAL_COURSE_ID])),nl,nl.
ecgi:-forall(courses_group_id(GROUP_ID,COURSE_ID,"E"),writef('[%w]%w\n',[GROUP_ID,COURSE_ID])),nl,nl.
ecgil:-forall(elective_courses_group_id_list(EGROUP_ID,ECOURSE_ID_LIST),writef('[%w]%w\n',[EGROUP_ID,ECOURSE_ID_LIST])),nl,nl.
mgi:-forall(major_group_id(MAJOR_ID,GROUP_ID,R_E),writef('[%w]%w\n',[MAJOR_ID,GROUP_ID,R_E])),nl,nl.
mgil:-forall(major_group_id_list(MAJOR_ID,ECOURSE_ID_LIST,R_E),writef('[%w]%w %w\n',[MAJOR_ID,ECOURSE_ID_LIST,R_E])),nl,nl.
mregil:-forall(major_regroup_id_list(MAJOR_ID,RCOURSE_ID_LIST,ECOURSE_ID_LIST),writef('[%w]%w %w\n',[MAJOR_ID,RCOURSE_ID_LIST,ECOURSE_ID_LIST])),nl,nl.

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

cm :- write('The current student is recommended to choose major:\n'),forall(choose_major(CSTU_ID,CSTU_NAME,M,MAJOR_NAME,Tm),writef('[%w]%w\t [%w]%w\t T(%w) = %w\n',[CSTU_ID,CSTU_NAME,M,MAJOR_NAME,MAJOR_NAME,Tm])),nl.
%% cml :- forall(choose_major_list(CStu,ML,Tm),writef('%w is recommended to choose major %w \n T%w = %w\n',[CStu,ML,MAJOR_NAME,Tm])),nl.

main :- open_odbc,
	cstu,
	%% fstu,
	%% fcid,
	%% fc,
	m,
%% 	cstu_gs,
%% 	fstu_gs,
%% 	fst_tc_pass,
%% 	fst_tc_fail,
%% 	ecgi,
%% 	ecgil,
%% 	mgi,
%% 	mgil,
%% 	mregil,
%% tfcall,
%% tfcfg,
%% tfcm,
%% csec,
%% csecl,
%% tfcfg,
%% tfcm,
%% bhfct,
%% bhfcetom,
%% bhmet,
%% bhfcp,
%% bhfcepom,
%% bhmep,
%% bhmrt,
%% bhmrp,
%% bhmt,
%% bhmp,
%% 	tm,
	cm,
	close_odbc.