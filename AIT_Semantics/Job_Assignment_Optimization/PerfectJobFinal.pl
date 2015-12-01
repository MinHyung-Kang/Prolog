% Sicstus Prolog Code
% Date : 2015/11/14
% Problem : Optimizing Job Assignment
% JobTypes : [bsf,hf,ib,su,gs] 
%            (bsf = Big Software Firm / hf = Hedge Fund / ib = Investment Bank /
%               su = Startup / gs = Grad School)
% Personalities : [mg,et,sl,ac]
%                   (mg = Money Grubber / et = Entreprenuer / sl = Slacker / ac = Academic)
% Relationships : [me(X,Y), f(X,Y), d(X,Y), m(X,Y)]
%                    (me = Mortal Enemies / f = friends / d = dating / m = married )
% Goal : optimizing the job allocation : maximize the sum of utilties

% Using clpfd library for constraint programming.
:- use_module(library(clpfd)).
:- use_module(library(lists)).


perfectJob(TotalUtility):-
	readInputs(People, Relationships, Offers),
	defineJobs(BSF, HF, IB, SU, GS),
	definePersonalities(MG, ET, SL, AC),
	getNamesAndPersonalities(People,[MG,ET,SL,AC], Names, Personalities),
	getUniqueJobsAndCities(Offers, UniqueJobList, UniqueCityList),
	getIndividualUtilities(Names, Personalities, Offers, [BSF, HF, IB, SU, GS], UniqueCityList, JobUtilities, JobList, JobCities),
	length(Names,N), length(Automaton,N),
	buildAutomaton(Automaton, Names, JobUtilities),
	restrainRelationships(Relationships, Automaton, Names, JobCities),	
	shave_List(Automaton),
	%maximize((solveProblem(Automaton, JobUtilities, Relationships, Names, JobCities, TotalUtility),
	% 			labeling([ffc,bisect],Automaton)),TotalUtility).
	maximize((solveProblem(Automaton, JobUtilities, Relationships, Names, JobCities, TotalUtility),
	 			labeling_with_shaving(Automaton,150,Automaton)),TotalUtility),
	print('\nAssignments : \n'),
	printAssignments(Names, Automaton, JobList).

%printAssignments(Names, Automaton, JobList)
printAssignments([], _, _).
printAssignments([N0|N1], [A0|A1], [JL0|JL1]):-
	mth1(A0, JL0, Job), 
	print(N0), print(' : '), print(Job), print('\n'),
	printAssignments(N1, A1, JL1).	
	
	
% solveProblem(?Automaton, +JobUtilities, +Relationships, +Names, +JobCities, ?TotalUtility)
% Computes the utility given the problems.
% Automaton : Job Assignment
% JobUtilities : Job utilities that match each job
% Relationships : All the relationship constraints.
% Names : list of all Names
% JobCities : Cities that match each job
% Total Utility : The total utility of this assignment.
solveProblem(Automaton, JobUtilities, Relationships, Names, JobCities, TotalUtility):-
	computeBaseUtility(Automaton, JobUtilities, BaseUtility),
	computeRelationshipUtility(Relationships, Automaton, Names, JobCities, ExtraUtility),
	TotalUtility #= BaseUtility + ExtraUtility.
	
	
	
% labeling_with_shaving(?X,+N,?Vars) : label the variables in Vars after every Nth assignment,
% shave the domain of variable X

labeling_with_shaving(X,N,Vars):-
	bb_put(1,0),
	labeling([value(shave_during_labeling(X,N))], Vars).
	
% shave_during_labeling(?X,+N,?V,?_Rest,?_BB0,?_BB) : X and N are given in the caller
% to labeling, V is the next Variable

shave_during_labeling(X,N,V,_Rest,_BB0,_BB):-
	labeling([ffc,bisect],[V]),
	bb_get(1,I),
	(	I<N -> I1 is I+1, bb_put(1,I1)
	;	shave_List(X),bb_put(1,0)
	).
	
% shave_List(?Automaton) : Shave the Automaton values
shave_List([]).
shave_List([A0|A1]):-
	shave_all(A0),
	shave_List(A1).

% shave_all(?X) : Shave every element of the list X
shave_all(X) :-
	fd_set(X, FD), fdset_to_list(FD, L),
	findall(X, member(X,L), Vs),
	list_to_fdset(Vs, FD1), X in_set FD1.

% readInputs(?People,?Relationships,?Offers) : Read all the input files
% People is a list of [People,Type], Relationships is a list of [x(Person,Person)],
% Offers is a list of [Person,Company,CompanyType,Location].
% The values each list can take are described in the header.
readInputs(People,Relationships,Offers):-
	_PplInput = 'C:/Users/user/IdeaProjects/PerfectJob/peopleinput6.txt',
	_RelInput = 'C:/Users/user/IdeaProjects/PerfectJob/relationshipsinput6.txt',
	_OffInput = 'C:/Users/user/IdeaProjects/PerfectJob/offersinput6.txt',
	readInput(_PplInput,People),
	readInput(_RelInput,Relationships),
	readInput(_OffInput,Offers).
	
% Read a particular input file
% readInput(+X,-Out ) : Out is string read from input X.
readInput(X,Out):-
	open(X,read,S),
	read(S,Out).
	
% defineJobs(?BSF, ?HF, ?IB, ?SU, ?GS) : Each variable describes the benefits and drawbacks of each job,
% in the form of [Pay, Hours, Impact, Opportunity to Learn]
defineJobs(BSF, HF, IB, SU, GS):-
	BSF = [6,6,2,8],
	HF = [8,8,4,6],
	IB = [10,10,3,4],
	SU = [4,8,10,8],
	GS = [1,4,3,10].

% definePersonalities(?MG, ?ET, ?SL, ?AC) : Each variable describes the sets of preferences (coefficients)
% of people for utility in order of [Pay, Hours, Impact, Opportunity to Learn]
definePersonalities(MG, ET, SL, AC):-
	MG = [10,-1,4,2],
	ET = [4,-2,10,8],
	SL = [1,-10,2,2],
	AC = [2,-6,8,10].
	
% getNamesAndPersonalities(+People, +UPerType, ?Names, ?Personalities) : People is the list of people and types,
% UPerType is a list of utilities per type, Names is the list of all the names (assumed unique)
% Personalities is the list of coefficients ordered by appearance of name
getNamesAndPersonalities([],_,[],[]).
getNamesAndPersonalities([P0|P1],[MG,ET,SL,AC], [N0|N1], [PS0|PS1]):-
	P0=[_Name,_Type], N0 = _Name,
	(	_Type = 'money_grubber' -> PS0 = MG
	; 	_Type = 'entrepreneur' -> PS0 = ET
	;	_Type = 'slacker' -> PS0 = SL
	;	_Type = 'academic' -> PS0 = AC
	), getNamesAndPersonalities(P1,[MG,ET,SL,AC], N1, PS1).
	
	
% getUniqueJobsAndCities(+Offers, ?JobList, ?JobTypeList, ?CityList) : 	
% Offers it the list of all the job offers, UniqueJobList is unique list of Jobs, 
% UniqueCityList is unique list of cities.
getUniqueJobsAndCities(Offers, UniqueJobList, UniqueCityList) :-
	getTotalJobsAndCities(Offers, TotalJobList, TotalCityList),
	getUniqueList(TotalCityList, UniqueCityList), 
	getUniqueList(TotalJobList, UniqueJobList).
	
% getTotalJobsAndCities(+Offers, ?TotalJobList, ?TotalCityList) : Gets list of all the jobs and cities	
getTotalJobsAndCities([],[],[]).	
getTotalJobsAndCities([O0|O1],[JL0|JL1],[CL0|CL1]):-
	O0 = [_, Job, _JobType, City],
	JL0 = Job, CL0 = City,
	getTotalJobsAndCities(O1,JL1,CL1).	
		
% getUniqueList(+TotalList, ?UniqueList)
% UniqueList is the list of unique elements from TotalList.
getUniqueList(TotalList, UniqueList):-
	getUniqueListHelper(TotalList,_,UniqueList).
	
% getUniqueListHelper(TotalList, Limits,UniqueList).
% TotalList is the list of all elements, Limits is the list of elements
% encountered during iteration, UniqueList is the unique list of elements in TotalList.
getUniqueListHelper([],_,[]).
getUniqueListHelper([TL0|TL1],L,UL):-
	(	testUnique(TL0,L) -> append([TL0],L,L1), 
		UL = [UL0|UL1], UL0 = TL0, getUniqueListHelper(TL1,L1,UL1)
	;	getUniqueListHelper(TL1,L,UL)	
	).

% testUnique(Element,Limits) : Fails if the Element is in the Limits.
testUnique(_,[]) :- !.
testUnique(Element,[L0|L1]):-
	(	Element = L0 -> fail
	; 	testUnique(Element,L1)	
	).

	
% getIndividualUtilities(Names, Personalities, Offers, JobTypes, UniqueCityList, JobUtilities, JobList, JobCities) : 
% Gets utilities and city index for each offer for each person
% JobUtilities is the list of list of utilites per each person that is obtained by choosing a job.
% JobCities is the list of list of cities per each person's offer in terms of index in UniqueCityList.
getIndividualUtilities([], _, _, _, _,[],[],[]).
getIndividualUtilities([N0|N1], [P0|P1], Offers, JobTypes, UniqueCityList, [JU0|JU1], [JL0|JL1],[JC0|JC1]) :-
	getUtilityForPerson(N0, P0, Offers, JobTypes, UniqueCityList, JU0, JL0, JC0),
	getIndividualUtilities(N1, P1, Offers, JobTypes, UniqueCityList, JU1, JL1, JC1).
	
% getUtilityForPerson(Name, Personality, Offers, JobTypes, UniqueCityList, JobUtilities, JobList, JobCities)
% Computes the list of utilities for person for each of his or her offers
% JobUtilities is the list of utilities obtained from each job option
% JobCities is the list of index of the cities that match the job option.
getUtilityForPerson(_, _ ,[], _, _, [], [], []).
getUtilityForPerson(Name, Personality,[O0|O1], JobTypes, UniqueCityList, JobUtilities, JobList, JobCities):-
	O0 = [_Name, Job, JobType, City], 
	(	Name = _Name -> JobUtilities = [JU0|JU1], JobCities = [JC0|JC1], JobList = [JL0|JL1],
		mth1(JobTypeIndex,['big_software_firm','hedge_fund','investment_bank','startup','grad_school'], JobType),
		mth1(JobTypeIndex, JobTypes, MatchingJobType),
		scalar_product(Personality, MatchingJobType, #=, JU0),
		JL0 = Job,
		mth1(JC0, UniqueCityList, City),
		getUtilityForPerson(Name, Personality, O1, JobTypes, UniqueCityList, JU1, JL1, JC1)
	;	getUtilityForPerson(Name, Personality, O1, JobTypes, UniqueCityList, JobUtilities, JobList, JobCities)
	).

	
	
% restrainRelationships(+Relationships, ?Automaton, +Names, +JobCities)
% Restrains the relationship so people who are married are assigned to same city
% and those who are mortal enemies are in different city.
restrainRelationships([], _, _, _).
restrainRelationships([R0|R1], Automaton, Names, JobCities):-
	(	R0 = m(X,Y) -> mustBeInSameCity(Automaton, Names, X, Y, JobCities)
	;	R0 = me(X,Y) -> cannotBeInSameCity(Automaton, Names, X, Y, JobCities)
	;	1 #= 1
	), restrainRelationships(R1, Automaton, Names, JobCities).

mustBeInSameCity(Automaton, Names, X, Y, JobCities):-
	mth1(FirstIndex, Names, X), mth1(SecondIndex, Names, Y),
	element(FirstIndex, Automaton, FirstCityIndex), element(SecondIndex, Automaton, SecondCityIndex),
	mth1(FirstIndex, JobCities, FirstCities), mth1(SecondIndex, JobCities, SecondCities), 
	element(FirstCityIndex, FirstCities, FirstCity), element(SecondCityIndex, SecondCities, SecondCity),
	FirstCity #= SecondCity.
	
cannotBeInSameCity(Automaton, Names, X, Y, JobCities):-
	mth1(FirstIndex, Names, X), mth1(SecondIndex, Names, Y),
	element(FirstIndex, Automaton, FirstCityIndex), element(SecondIndex, Automaton, SecondCityIndex),
	mth1(FirstIndex, JobCities, FirstCities), mth1(SecondIndex, JobCities, SecondCities), 
	element(FirstCityIndex, FirstCities, FirstCity), element(SecondCityIndex, SecondCities, SecondCity),
	FirstCity #\= SecondCity.
	


% buildAutomaton (?JobAutomaton, +Names, +JobUtilities) : builds Automaton with given names and jobs
% JobAutomaton : The list of numbers which indicate each job assignment
% Names : list of all names, JobUtilities : List of all job utilities for each person
buildAutomaton(JobAutomaton, Names, JobUtilities):-
	buildAutomatonForPeople(Names, JobUtilities, ArcList),
	Names = [N0|_],
	length(Names, N),
	length(JobAutomaton, N),
	automaton(JobAutomaton,[source(N0),sink(goal)], ArcList).
	
% buildAutomatonForPeople(+Names, +JobUtilities, ?ArcList) : 
% AddArcs for each person.
% Names : list of all names, JobUtilities : List of all job utilities for each person
% ArcList : list of arcs for the automaton
buildAutomatonForPeople([],_,[]).
buildAutomatonForPeople([N0|N1],[JU0|JU1],ArcList):-
	(	N1 = []	-> addArcsForPerson(N0,goal,JU0,ArcList1, _Count)
	;	N1 = [N10|_], addArcsForPerson(N0,N10,JU0,ArcList1, _Count)	
	), append(ArcList1,ArcList2,ArcList),
	buildAutomatonForPeople(N1,JU1,ArcList2).
			
% addArcsForPerson(+N0,+N1,+JobUtilities,?ArcList, ?Count)
% Add arcs for each person.
% N0 : Person of interest N1 : Next person on the list
% JobList : list of Job offers for this person
% ArcList : list of arcs for the automaton
% Count : Count of the utilites to create jobautomaton
addArcsForPerson(_,_,[],[],0).
addArcsForPerson(N0,N1,[_JL0|JL1],[AL0|AL1],Count):-
	AL0 = arc(N0,Count,N1), 
	Count #= Count2 + 1,
	addArcsForPerson(N0,N1,JL1,AL1, Count2).

	
% computeBaseUtility(?Automaton, +JobUtilities, ?BaseUtility). : 
% Given the Automaton, compute the combined Baseutility for everyone

computeBaseUtility([],_,0).
computeBaseUtility([A0|A1],[JU0|JU1],BU):-
	element(A0, JU0, Utility),
	BU #= Utility + BU2,
	computeBaseUtility(A1, JU1, BU2).

	
	
% computeRelationshipUtility(Relationships,Automaton, Names,  JobCities, ExtraUtility)
% Computes the additional utilities gained from relationships.
% ExtraUtility is the utility gained from relationships assigned by this Automaton.
computeRelationshipUtility([],_,_,_,0).
computeRelationshipUtility([R0|R1], Automaton, Names,  JobCities, ExtraUtility):-
	(	R0 = f(X,Y) -> twoInSameCity(Automaton, Names, X, Y, JobCities, SameCity),
		Extra #= SameCity * 40
	;	R0 = d(X,Y) ->  twoInSameCity(Automaton, Names, X, Y, JobCities, SameCity),
		Extra #= SameCity * 100
	;	Extra #= 0 
	), ExtraUtility #= Extra + ExtraUtility2,
	computeRelationshipUtility(R1, Automaton, Names, JobCities, ExtraUtility2).
	
% twoInSameCity(?Automaton, +Names, +X, +Y, +JobCities, ?SameCity)
% SameCity is 1 iff X and Y are assigned to the same city.
twoInSameCity(Automaton, Names, X, Y, JobCities, SameCity):-
	mth1(FirstIndex, Names, X), mth1(SecondIndex, Names, Y),
	element(FirstIndex, Automaton, FirstCityIndex), element(SecondIndex, Automaton, SecondCityIndex),
	mth1(FirstIndex, JobCities, FirstCities), mth1(SecondIndex, JobCities, SecondCities),
	element(FirstCityIndex, FirstCities, FirstCity), element(SecondCityIndex, SecondCities, SecondCity),
	FirstCity #= SecondCity #<=> SameCity.
	
% mth1(?Index, ?List, ?Val)	: Val is the value of the List at index Index.
% Implemented to get rid of choice point
mth1(1,[Val|_],Val) :- !.
mth1(Index,[_|L1],Val) :-
    Index #> 1,
    Index1 #= Index-1,
    mth1(Index1,L1,Val).