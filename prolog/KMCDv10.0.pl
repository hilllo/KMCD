open_odbc:-
%% Create a new ODBC connection to data-source DSN and return a handle to this connection in Connection.
	odbc_connect('cpl_kmcd',_,
		[ user('root'),
		password(''),
		alias('kmcd_connection'),
		open('once')]
	).
	
close_odbc:-
	odbc_disconnect(kmcd_connection).

:-open_odbc.

%% ---------------------------------------------------
%% KEY FACTS to KMCD read from SQL
%% ---------------------------------------------------
:- dynamic current_stu/2.
current_stu_get(CSTU_ID,CSTU_NAME):-
	odbc_query(kmcd_connection,'
		select distinct stu_id,stu_name 
		from cpl_kmcd_stu stu
		where stu.stu_prospect="unknown"
		',
		row(CSTU_ID,CSTU_NAME))
	.

:- dynamic former_stu/2.
former_stu_get(FSTU_ID,FSTU_NAME):-
	%% odbc_current_connection(Connection,_),
	odbc_query(kmcd_connection,'
		select distinct stu_id,stu_name 
		from cpl_kmcd_stu stu
		where stu.stu_prospect="graduated"',
		row(FSTU_ID,FSTU_NAME)
		).

:- dynamic major/2.
major_get(MAJOR_ID,MAJOR_NAME) :-
	%% odbc_current_connection(Connection,_),
	odbc_query(kmcd_connection,'
		select distinct major.major_id, major.major_name
		from cpl_kmcd_major major',
		row(MAJOR_ID,MAJOR_NAME)
		).

:- dynamic cstu_course_id/2.
cstu_course_id_get(CSTU_ID,CSTU_COURSE_ID) :-
	%% odbc_current_connection(Connection,_),
	odbc_query(kmcd_connection,'
		select stu_id,course_id
		from cpl_kmcd_completed_course cc',
		row(CSTU_ID,CSTU_COURSE_ID)
		).

:- dynamic optional_future_course/2.
optional_future_course_get(CSTU_ID,OPTIONAL_COURSE_ID) :-
	%% odbc_current_connection(Connection,_),
	odbc_query(kmcd_connection,'
		select distinct stu_id,course_id
		from cpl_kmcd_future_courses',
		row(CSTU_ID,OPTIONAL_COURSE_ID)
		).

:-dynamic get_score/2.
get_score_get(STU_ID,COURSE_ID,COURSE_SCORE) :-
	%% odbc_current_connection(Connection,_),
	odbc_query(kmcd_connection,'
		select stu_id,course_id,course_score
		from cpl_kmcd_stu_scoreofcourse',
		row(STU_ID,COURSE_ID,COURSE_SCORE)
		).
	

%% courses' score of current students
cstu_get_score(CSTU_ID,CSTU_COURSE_ID,CSTU_COURSE_SCORE) :-
	current_stu(CSTU_ID,_)
	,get_score(CSTU_ID,CSTU_COURSE_ID,CSTU_COURSE_SCORE).

%% future courses which has been token by previous students.
%% fstu_take_course(FSTU_ID,OPTIONAL_COURSE_ID,COURSE_STATUS)
%% COURSE_STATUS=1 means pass, COURSE_STATUS=0 means fail.
fstu_take_course(FSTU_ID,OPTIONAL_COURSE_ID,P_F):-
	get_score(FSTU_ID,OPTIONAL_COURSE_ID,COURSE_SCORE ),
	((COURSE_SCORE >= 74, COURSE_SCORE =< 100) -> P_F = 1; P_F = 0).



%% elective courses groups
:-dynamic courses_group_id/3.
courses_group_id_get(CSTU_ID,GROUP_ID,COURSE_ID,R_E) :-
	odbc_query(kmcd_connection,'
	select stu_id,group_id,course_id,r_e
	from cpl_kmcd_future_major_courses',
	row(CSTU_ID,GROUP_ID,COURSE_ID,R_E)
	).

:-dynamic group_id/2.
group_id_get(CSTU_ID,GROUP_ID):-
	odbc_query(kmcd_connection,'
	select distinct stu_id,group_id
	from cpl_kmcd_future_major_courses',
	row(CSTU_ID,GROUP_ID)
	).

:-dynamic major_group_id/4.
major_group_id_get(CSTU_ID,MAJOR_ID,GROUP_ID,R_E) :-
	%% odbc_current_connection(Connection,_),
	odbc_query(kmcd_connection,'
	select stu_id,major_id,group_id,r_e
	from cpl_kmcd_future_major_courses
	group by stu_id,major_id,group_id',
	row(CSTU_ID,MAJOR_ID,GROUP_ID,R_E)
	).

initialize:-
	forall(current_stu_get(CSTU_ID,CSTU_NAME),assert(current_stu(CSTU_ID,CSTU_NAME)))
	,forall(former_stu_get(FSTU_ID,FSTU_NAME),assert(former_stu(FSTU_ID,FSTU_NAME)))
	,forall(cstu_course_id_get(CSTU_ID,CSTU_COURSE_ID),assert(cstu_course_id(CSTU_ID,CSTU_COURSE_ID)))
	,forall(optional_future_course_get(CSTU_ID,OPTIONAL_COURSE_ID),assert(optional_future_course(CSTU_ID,OPTIONAL_COURSE_ID)))
	,forall(get_score_get(STU_ID,COURSE_ID,COURSE_SCORE),assert(get_score(STU_ID,COURSE_ID,COURSE_SCORE)))
	,forall(courses_group_id_get(CSTU_ID,GROUP_ID,COURSE_ID,R_E),assert(courses_group_id(CSTU_ID,GROUP_ID,COURSE_ID,R_E)))
	,forall(group_id_get(CSTU_ID,GROUP_ID),assert(group_id(CSTU_ID,GROUP_ID)))
	,forall(major_get(MAJOR_ID,MAJOR_NAME),assert(major(MAJOR_ID,MAJOR_NAME)))
	,forall(major_group_id_get(CSTU_ID,MAJOR_ID,GROUP_ID,R_E),assert(major_group_id(CSTU_ID,MAJOR_ID,GROUP_ID,R_E)))
	.
:-initialize.

%% ---------------------------------------------------
%% KEY RULES to KBMC
%% ---------------------------------------------------

%% -----------
%% Tfc
%% -----------
/*Find out former students FStu who have similar capabilities
 with the current student CSTU_ID.*/
similar_fstu(CSTU_ID,FSTU_ID):-
	cstu_get_score(CSTU_ID,CSTU_COURSE_ID,CSTU_COURSE_SCORE)
	,get_score(FSTU_ID,CSTU_COURSE_ID,FSTU_COURSE_SCORE)
	,FSTU_COURSE_SCORE=<CSTU_COURSE_SCORE+2,FSTU_COURSE_SCORE>=CSTU_COURSE_SCORE-2
	.



/*Find out future courses TC that those former students FSTU_ID 
have been token.*/
similar_fstu_take_course(CSTU_ID,FSTU_ID,FSTU_COURSE_ID,P_F):-
	setof(FSTU_ID,similar_fstu(CSTU_ID,FSTU_ID),FSTU_ID_LIST),
	member(FSTU_ID,FSTU_ID_LIST),
	fstu_take_course(FSTU_ID,FSTU_COURSE_ID,P_F)
	.


/*Count the number BhfcT and BhfcP of those former student who have been 
taken or passed course OFC as a support for the current student CSTU_ID which 
are brought by both the hypothesis and the evidence.*/
bhfc_take_pass(CSTU_ID,OFC,BhfcT,BhfcP):-
	current_stu(CSTU_ID,_),
	optional_future_course(CSTU_ID,OFC),
	findall(P_F,similar_fstu_take_course(CSTU_ID,_,OFC,P_F),P_F_L)
	,length(P_F_L,BhfcT)
	,sum_list(P_F_L,BhfcP)
	.


/*Calculate the supporting degree Tfc of the future courses OFC.*/
tfc(CSTU_ID,OFC,Tfc_FLOAT,BhfcT,BhfcP):-
	bhfc_take_pass(CSTU_ID,OFC,BhfcT,BhfcP),
	(BhfcT=\=0 ->
	Tfc = BhfcP/BhfcT;
	Tfc = 0),
	Tfc_C = integer(Tfc*100) + BhfcT/200,
	Tfc_FLOAT is float(Tfc_C)
	.

/*Calculate the supporting degree of courses in a gourp GROUP_ID.*/
tfc_for_group(CSTU_ID,GROUP_ID,OFC,Tfc_FLOAT,BhfcT,BhfcP):-
	current_stu(CSTU_ID,_)
	,courses_group_id(CSTU_ID,GROUP_ID,OFC,_)
	,tfc(CSTU_ID,OFC,Tfc_FLOAT,BhfcT,BhfcP)
	.

/*Calculate the max supporting degree of courses in a gourp GROUP_ID.*/
tfc_max(CSTU_ID,GROUP_ID,TfcM):-
	current_stu(CSTU_ID,_)
	,group_id_get(CSTU_ID,GROUP_ID)
	,findall(Tfc_FLOAT,tfc_for_group(CSTU_ID,GROUP_ID,_,Tfc_FLOAT,_,_),Tfc_FLOAT_L)
	,max_member(TfcM,Tfc_FLOAT_L)
	.

/*Find all recommended course's id whose Tfc equals to the maximum Tfc in the group.*/
choose_course_list(CSTU_ID,GROUP_ID,TfcM,OFC_L):-
	tfc_max(CSTU_ID,GROUP_ID,TfcM)
	,findall(OFC,tfc_for_group(CSTU_ID,GROUP_ID,OFC,TfcM,_,_),OFC_L)
	.

/*Get other information of the recommended course for further calculation.*/
choose_course(CSTU_ID,GROUP_ID,OFC,BhfcT,BhfcP,TfcM):-
	choose_course_list(CSTU_ID,GROUP_ID,TfcM,OFC_L),
	OFC_L=[OFC|_],
	tfc(CSTU_ID,OFC,_,BhfcT,BhfcP)
	.
	
%%Set recommended courses' information as evidence to raise the speed of searching.
:-dynamic choose_course_fast/6. 
savecc:-
	forall(choose_course(CSTU_ID,GROUP_ID,OFC,BhfcT,BhfcP,TfcM),assert(choose_course_fast(CSTU_ID,GROUP_ID,OFC,BhfcT,BhfcP,TfcM)))
	.
:-savecc.

%% -----------
%% Tm
%% -----------
/*list out all groups GROUP_ID and its max BhfcP/BhfcT in a major MAJOR_ID*/
tfc_for_major(CSTU_ID,MAJOR_ID,GROUP_ID,BhfcT,BhfcP):-
	major(MAJOR_ID,_),
	major_group_id_get(CSTU_ID,MAJOR_ID,GROUP_ID,_),
	choose_course_fast(CSTU_ID,GROUP_ID,_,BhfcT,BhfcP,_)
	.


bhm_take_pass(CSTU_ID,MAJOR_ID,BhmT,BhmP):-
	current_stu(CSTU_ID,_),
	major(MAJOR_ID,_),
	findall(BhfcT,tfc_for_major(CSTU_ID,MAJOR_ID,_,BhfcT,_),BhfcT_L),
	findall(BhfcP,tfc_for_major(CSTU_ID,MAJOR_ID,_,_,BhfcP),BhfcP_L),
	sum_list(BhfcT_L,BhmT),
	sum_list(BhfcP_L,BhmP)
	.

tm(CSTU_ID,MAJOR_ID,BhmT,BhmP,Tm_FLOAT):-
	current_stu(CSTU_ID,_),
	major(MAJOR_ID,_),
	findall(BhfcT,tfc_for_major(CSTU_ID,MAJOR_ID,_,BhfcT,_),BhfcT_L),
	findall(BhfcP,tfc_for_major(CSTU_ID,MAJOR_ID,_,_,BhfcP),BhfcP_L),
	sum_list(BhfcT_L,BhmT),
	sum_list(BhfcP_L,BhmP),
	(BhmT=\=0 ->
	Tm=BhmP/BhmT;
	Tm=0),
	Tm_C = integer(Tm*100) + BhmT/200,
	Tm_FLOAT is float(Tm_C)
	.
	
tm_max(CSTU_ID,TmM):-
	findall(Tm_FLOAT,tm(CSTU_ID,_,_,_,Tm_FLOAT),Tm_L),
	max_member(TmM,Tm_L).
	
choose_major(CSTU_ID,MAJOR_NAME):-
	current_stu(CSTU_ID,_),
	tm_max(CSTU_ID,TmM),
	Tm_FLOAT is TmM,
	tm(CSTU_ID,MAJOR_ID,_,_,Tm_FLOAT),
	major(MAJOR_ID,MAJOR_NAME).


cstu(CSTU_ID) :- 
	write('Current Students:\nstudent_id\tstudent_name\n===============================================\n'),
	forall(current_stu(CSTU_ID,CSTU_NAME),writef('%w\t%w\n',[CSTU_ID,CSTU_NAME])),nl,nl.
cstu:-cstu(CSTU_ID).

fc:- 
	write('Future Courses:\ncourse_id\tcourse_name\n===============================================\n'),
	forall(optional_future_course(CSTU_ID,OPTIONAL_COURSE_ID),writef('%w\t%w\n',[CSTU_ID,OPTIONAL_COURSE_ID])),nl,nl.

m :- 
	write('Optional Major:\nmajor_id\tmajor_name\n===============================================\n'),
	forall(major(MAJOR_ID,MAJOR_NAME),writef('%w\t\t%w\n',[MAJOR_ID,MAJOR_NAME])),nl,nl.

cstu_gs :- 
	write('Current courses & scores of current students:\nstudent_id\tcourse_id\tscore\n===============================================\n'),
	forall(cstu_get_score(CSTU_ID,CSTU_COURSE_NAME,CSTU_COURSE_SCORE),writef('%w\t%w\t%w \n',[CSTU_ID,CSTU_COURSE_NAME,CSTU_COURSE_SCORE])),nl,nl.

sfstu(CSTU_ID) :- 
	write('Similar former students:\ncstudent_id\tfstudent_id\n===============================================\n'),
	forall(similar_fstu(CSTU_ID,FSTU_ID),writef('%w\t%w\n',[CSTU_ID,FSTU_ID])),nl,nl.
sfstu :- sfstu(CSTU_ID).

sfst_tc_take :-
	write('Future courses that similar former students has taken:\ncstu_id\t\tfstu_id\t\tcourse_id(take)\n===============================================\n'),
	forall(similar_fstu_take_course(CSTU_ID,FSTU_ID,FSTU_COURSE_ID,_),writef('%w\t%w\t%w\n',[CSTU_ID,FSTU_ID,FSTU_COURSE_ID])),nl,nl.

bhfc(CSTU_ID) :- 
	write('The number of similar former students who took/passed future courses:\nstudent_id\tcourse_id\tBh1(fc)\tBh2(fc)\n===============================================\n'),
	forall(bhfc_take_pass(CSTU_ID,OFC,BhfcT,BhfcP),writef('%w\t%w\t%w\t%w\n',[CSTU_ID,OFC,BhfcT,BhfcP])),nl.
bhfc :- bhfc(CSTU_ID).

tfcall(CSTU_ID) :- 
	write('The supporting degrees of the future elective courses fc:\nstudent_id\tcourse_id\tT(fc)\n===============================================\n'),
	forall(tfc(CSTU_ID,OFC,Tfc_FLOAT,_,_),writef('%w\t%w\t%w\n',[CSTU_ID,OFC,Tfc_FLOAT])),nl.
tfcall :- tfcall(CSTU_ID).

tfcfg :- 
	write('The supporting degrees of every future elective courses in groups:\ngroup_id\tcourse_id\tT(fc)\n======================================================================\n'),
	forall(tfc_for_group(_,GROUP_ID,OFC,Tfc_FLOAT,_,_),writef('%w\t%w\t%w\t\n',[GROUP_ID,OFC,Tfc_FLOAT])),nl.
tfcmax :- 
	write('The recommended courses for group is:\ngroup_id\tT(fc)\n===========================================\n'),
	forall(tfc_max(_,ECGNum,TfcM),writef('%w\t\t%w\n',[ECGNum,TfcM])),nl.

ccl :- 
	write('The recommended courses for elective courses group:\nstu_id\t\tgroup_id T(fc)\tcourse_id\n============================================================\n'),
	forall(choose_course_list(CSTU_ID,GROUP_ID,TfcM,OFC_L),writef('%w\t%w\t%w\t\t%w\n',[CSTU_ID,GROUP_ID,TfcM,OFC_L])),nl.

cc(CSTU_ID) :- 
	write('The recommended courses for elective courses group:\nstu_id\t\tgroup_id\tcourse_id\n===========================================\n'),
	forall(choose_course(CSTU_ID,GROUP_ID,OFC,_,_,_),writef('%w\t%w\t\t%w\n',[CSTU_ID,GROUP_ID,OFC])),nl.
cc :- cc(CSTU_ID).

tfcfm:-
	write('the supporting degree of choosing the major m:\nmajor_id\tT(m)\n===========================================\n'),
	forall(tfc_for_major(CSTU_ID,MAJOR_ID,GROUP_ID,BhfcT,BhfcP),writef('%w\t\t%w\t%w\t%w\n',[CSTU_ID,MAJOR_ID,GROUP_ID,BhfcT,BhfcP])),nl.
%% csec :- 
%% 	write('The recommended courses for group is:\ngroup_id\tcourse_id\tT(fc)\n===========================================\n'),
%% 	forall(cstu_select_elective_course(_,ECGNum,SC,Tfc),writef('%w\t\t%w\t%w\n',[ECGNum,SC,Tfc])),nl.

bhmtp(CSTU_ID) :- 
	write('The number of similar former students who took future courses in majors:\nstudent_id\tmajor_id\tB1h(m)\tB2h(m)\n===========================================\n'),
	forall(bhm_take_pass(CSTU_ID,MAJOR_ID,BhmT,BhmP),writef('%w\t%w\t\t%w\t%w\n',[CSTU_ID,MAJOR_ID,BhmT,BhmP])),nl.
bhmtp :- bhmtp(CSTU_ID).

tm(CSTU_ID) :- 
	write('the supporting degree of choosing the major m:\nstudent_id\tmajor_id\tT(m)\n===========================================\n'),
	forall(tm(CSTU_ID,MAJOR_ID,_,_,Tm_FLOAT),writef('%w\t%w\t\t%w\n',[CSTU_ID,MAJOR_ID,Tm_FLOAT])),nl.
%% tml :- forall(tm_list(_,TmL),writef('Tm = %w\n',[TmL])),nl.
%% tmm :- forall(tm_max(_,TmM),writef('TmMax = %w\n',[TmM])),nl.
tm :- tm(CSTU_ID).

cm(CSTU_ID) :- 
	write('The current student is recommended to choose major:\nstudent_id\tmajor_name\n===========================================\n'),
	forall(choose_major(CSTU_ID,M),writef('%w\t%w\t\n',[CSTU_ID,M])),nl.
%% cml :- forall(choose_major_list(CStu,ML,Tm),writef('%w is recommended to choose major %w \n T%w = %w\n',[CStu,ML,MAJOR_NAME,Tm])),nl.

%% ---------------------------------------------------
%% EXPLAINATION
%% ---------------------------------------------------
explain(1):-cstu.
explain(2):-sfstu.
explain(3):-sfst_tc_take.
explain(4):-sfst_tc_pass.