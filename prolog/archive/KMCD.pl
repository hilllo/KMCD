:-dynamic(get_mark/3,take_course/2).


%% new students' marks
get_mark(jack,maths,80).
get_mark(jack,physics,80).

%% privious students' marks
get_mark(tom,maths,80).
get_mark(tom,physics,90).
get_mark(mike,maths,85).
get_mark(mike,physics,85).
get_mark(rose,maths,100).
get_mark(rose,physics,100).

%% privious students' course
take_course(tom,maths).
take_course(tom,physics).
take_course(tom,java).
take_course(tom,database).
take_course(mike,maths).
take_course(mike,physics).
take_course(mike,operating_system).
take_course(rose,maths).
take_course(rose,physics).
take_course(rose,c_plus_plus).

%% major and courses groups
major(network_engineering,[java,operating_system],[database]).
major(software_engineering,[java,operating_system],[c_plus_plus]).


%% find out which courses student A may pass
%% A:new student A. FC:future course that A may pass. C:courses that A has chosen. M1/M2: marks of group C.  
%% test(A,B,C):-get_mark(A,C,M1),get_mark(B,C,M2),A\=B,M2<M1+5.1,M2>M1-5.1.
pass(A,FC):-
			get_mark(A,C,M1),get_mark(B,C,M2),A\=B,(M2<M1+5;M2=M1+5),(M2>M1-5;M2=M1+5),
			take_course(B,FC).

			
choose_major(A,M):-major(M,C1,C2),pass(A,FC1),member(FC1,C1),pass(A,FC2),member(FC2,C2).

/*20140528			
count(0,[]).
count(Count,[Head|Tail]):- count(TailCount,Tail),Count is TailCount+1.
test(a,[x,y,z]).
test(jack,List):- pass(jack,B),List=[List,B]. 
ctest(Count):-test(a,B),count(Count,B).
*/

%% RECORD
/*NEW STUDENTS
NAME						COURSE						MARKS
jack						maths						80
							physics						80
*/

/*PRIVIOUS STUDENTS
NAME						COURSE						MARKS
tom							maths						80
							physics						90
							java						--
							database					--
---------------------------------------------------------------------------
mike						maths						85
							physics						85
							operating_system			--
---------------------------------------------------------------------------
rose						maths						100
							physics						100
							c_plus_plus					--	
*/

/*COURSES' DATA OF PRIVIOUS STUDENTS
COURSE						NAME						MARKS
maths						tom							80
							mike						85
							rose						100
---------------------------------------------------------------------------
physics						tom							90
							mike						85
							rose						100
---------------------------------------------------------------------------
java						tom							--
---------------------------------------------------------------------------
database					tom							--
---------------------------------------------------------------------------
operating_system			mike						--
---------------------------------------------------------------------------
c_plus_plus					rose						--
---------------------------------------------------------------------------
*/

/*MAJOR
MAJOR						GROUP1						GROUP2
network_engineering			java						database
							operating_system
---------------------------------------------------------------------------
software_engineering		java						c_plus_plus
							operating_system
*/
