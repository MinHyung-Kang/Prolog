
:- use_module(library(clpfd)).
:- use_module(library(lists)).


    
% skysudoku(+SP,-SS) : SS is a solution of the Skyscrpaer Sudoku puzzle SP.	
	
skysudoku(ss(K,Clues),SS):-
	% K=2,Clues = [v(4,n,1),g(4,1,4)],
	% K=3, Clues = [g(1,1,1),g(9,1,4),g(6,1,5),g(5,1,9),g(2,3,3),g(4,3,5),g(3,5,2),g(4,5,3),g(7,5,7),g(2,6,4),g(6,6,6),g(1,6,9),g(2,7,1),g(5,7,6),g(7,8,2),g(3,8,8),g(1,9,6),v(2,e,8),v(2,e,9),v(3,w,1),v(3,n,6),v(2,s,3),v(3,w,5),v(2,n,8),v(3,n,9),v(3,e,1),v(3,e,2),v(2,e,3),v(1,e,4)],
    % K = 4,Clues = [v(3,w,2),v(4,n,12),v(2,n,13),v(3,e,6),v(2,e,4),g(5,1,11),g(13,1,15),g(14,1,16),g(13,2,6),g(6,2,7),g(12,2,8),g(16,2,10),g(14,2,11),g(3,2,12),g(7,2,15),g(1,3,2),g(5,3,4),g(7,3,7),g(2,3,13),g(11,3,14),g(5,4,5),g(4,4,9),g(12,4,12),g(6,4,14),g(16,4,15),g(5,5,2),g(15,5,3),g(9,5,4),g(1,5,6),g(11,5,9),g(14,5,10),g(6,6,3),g(7,6,6),g(10,6,7),g(8,6,12),g(13,6,14),g(2,7,3),g(4,7,5),g(12,7,6),g(16,7,14),g(8,8,1),g(16,8,3),g(3,8,7),g(15,8,8),g(9,8,10),g(1,8,13),g(5,8,15),g(16,9,1),g(12,9,9),g(4,9,11),g(9,9,14),g(6,10,1),g(3,10,2),g(14,10,4),g(7,10,5),g(12,10,15),g(9,11,1),g(13,11,3),g(15,11,4),g(10,11,9),g(7,11,10),g(14,11,14),g(4,11,16),g(4,12,1),g(8,12,5),g(16,12,6),g(9,12,7),g(15,12,9),g(13,12,10),g(6,12,11),g(3,13,1),g(10,13,3),g(1,13,4),g(2,13,10),g(4,13,12),g(7,13,13),g(9,13,15),g(2,14,7),g(8,14,11),g(7,14,12),g(14,14,13),g(5,14,14),g(3,14,15),g(12,14,16),g(15,15,1),g(13,15,2),g(2,15,4),g(6,15,6),g(3,15,9),g(1,15,11),g(8,16,3),g(12,16,4),g(11,16,6),g(9,16,9),g(6,16,13),g(10,16,16)],
	 K = 3, Clues = [v(2,n,2),v(3,n,4),v(2,n,6),v(2,n,8),v(3,e,1),v(4,e,3),v(2,e,5),v(5,e,7),v(2,e,9),v(3,s,2),v(3,s,4),v(4,s,6),v(2,w,1),v(2,w,3),v(4,w,6),v(2,w,9),g(5,1,3),g(1,2,2),g(3,2,7),g(4,2,8),g(6,4,5),g(2,4,6),g(1,5,3),g(5,5,7),g(3,6,5),g(6,8,2),g(3,8,3),g(4,9,4),g(3,9,9)],
	% K=3, Clues = [v(5,n,2),v(4,n,4),v(5,n,6),v(2,n,7),v(2,n,8),v(2,n,9), v(4,w,4),v(8,w,5),v(2,w,6),v(3,w,7),v(3,w,9), v(4,s,4),v(2,s,7),v(2,s,8),v(3,s,9), v(3,e,3),v(3,e,4),v(2,e,6),v(6,e,8), g(6,3,1),g(7,3,6),g(2,6,5),g(7,7,4),g(4,8,6)],
	basicDimensions(K,SS,SST),
	M #= K * K,
	createList(M,L),
	putConstraintClues(Clues,SS,SST),
	%shave_sudoku(SS),
	basicConsistency(K,SS,SST,L),
	%shave_sudoku(SS),
	append(SS,_Vars),
	%labeling_with_shaving(SS,1000,_Vars).
	labeling([ffc,enum],_Vars).

	
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
	;	shave_sudoku(X),bb_put(1,0)
	).

	
	
% shave_sudoku (?SS) : Shave the domains of the Sudoku SS

shave_sudoku([]).
shave_sudoku([SS0|SS1]):-
	shave_row(SS0),
	shave_sudoku(SS1).


	
% shave_row(?SS) : Shave each row of Sudoku SS.

shave_row([]).
shave_row([SS0|SS1]):-
	shave_all(SS0),
	shave_row(SS1).


	
% shave_all(?X) : Shave every element of the list X

shave_all(X) :-
	fd_set(X, FD), fdset_to_list(FD, L),
	findall(X, member(X,L), Vs),
	list_to_fdset(Vs, FD1), X in_set FD1.


	
% basicDimensions(+K,?SS,?SST) : set up the basic domains of sudoku SS,
% whose size is K^2 x K^2. SST is the transposed matrix of SS.	

basicDimensions(K,SS,SST):-
	M #= K * K,
	length(SS,M),
	length(SST,M),
	constrainDimensions(M,SS),
	transpose(SS,SST),
	constrainDimensions(M,SS).

	
	
% constrainDimensions (+M,?SS) : M is the size of width and height of sudoku SS.
% Constrains each row of the sudoku so that each row have length M and the values
% range from 1 to M.

constrainDimensions(_,[]).
constrainDimensions(M,[SS0|SS1]):-
	length(SS0,M),
	domain(SS0,1,M),
	constrainDimensions(M,SS1).

	
	
% basicConsistency(+K,?SS,?SST,+L) : SS is the sudoku of size K^2 x K^2.
% SST is the transposed matrix of SS, and L is the list of values from 1 to K^2
% which is used to set the global_cardinality of each row.
% constrains each row, column, and subgrid to be consistent.	

basicConsistency(K,SS,SST,L):-
	M #= K * K,
	constrainConsistency(SS,L),
    constrainConsistency(SST,L),
	length(SSGrid,M),
	getSubGrid(SS,K,SSGrid),
	consistentRow(M,SSGrid,L).


	
% constrainConsistency(?SS,+L) : SS is the sudoku where for each row,
% the elements of unique values in L.

constrainConsistency([],_).
constrainConsistency([SS0|SS1],L):-
	%all_different(SS0),
	global_cardinality(SS0,L),
	constrainConsistency(SS1,L).
		  

		  
% consistentRow(+M,?SS, +L) : each element of SS is a row with all its elements
% unique between 1 and M, and each row is of length M.  
% Elements of each row has unique values from L

consistentRow(_,[],_).
consistentRow(M,[SS0|SS1],L):-
    length(SS0,M),
    domain(SS0,1,M),
	global_cardinality(SS0,L),
    consistentRow(M,SS1,L).


	
% createList(+M, ?L) : L is a list of 1 ... M.
% Used to set global_cardinality constraint

createList(0,[]):-!.
createList(M,[L0|L1]):-
	L0 = M-1,
	M2 is M - 1,
	createList(M2,L1).


    
% putconstraintclues(+Clues,?SS, ?SST) : K is the number size of subgrid of the
% sudoku SS, and clues tell us information about the sudoku SS.
% SST is the transposed matrix of SS.

putConstraintClues([],_,_).
putConstraintClues([L0|L1],SS,SST):-
    (  L0 = g(N,R,C) -> givenNumber(N,R,C,SS)
    ;  L0 = v(V,Dir,RC), visible(V,Dir,RC,SS,SST)
    ),
	putConstraintClues(L1,SS,SST).    
  

  
% givennumber(+N,+R,+C,?SS) : Number N is known to occupy row R column C
% of sudoku SS

givenNumber(N,R,C,SS):-
    mth1(R,SS,_SS1),
    element(C,_SS1, N).

	
	
% visible(+V,+Dir,+RC,?SS,?SST): Dir specifies the direction from which the visible
% buildings are counted. Dir is one of the atoms n,e,s,w. RC is a row or
% column number to which the clue applies. V is the count of the buildings
% visible from the direction Dir in row or column RC in SS.    
% SST is the transposed matrix of SS.

visible(V,Dir,RC,SS,SST):-
	( Dir = e -> mth1(RC,SS,RowCol),
		reverse(RowCol,RowColNew), visnum(RowColNew,V)
	; Dir = w, mth1(RC,SS,RowCol),
		visnum(RowCol,V)
	; Dir = s, mth1(RC,SST,RowCol),
		reverse(RowCol,RowColNew), visnum(RowColNew,V)
	; Dir = n, mth1(RC,SST,RowCol),
		visnum(RowCol,V)
	),!.
	
	
visible3(V,Dir,RC,SS,SST):-
	( Dir = e -> mth1(RC,SS,RowCol),
		reverse(RowCol,RowColNew), visibleNum(RowColNew,V)
	; Dir = w, mth1(RC,SS,RowCol),
		visibleNum(RowCol,V)
	; Dir = s, mth1(RC,SST,RowCol),
		reverse(RowCol,RowColNew), visibleNum(RowColNew,V)
	; Dir = n, mth1(RC,SST,RowCol),
		visibleNum(RowCol,V)
	),!.
	     
    
	
% visnum(+L, ?K): the nubmer of elements in list L that are visible
% from the left is K.

visnum([],0).
visnum([L1|L2],K):-
     visnum1(L2,L1,K).

% visnum1(+L, +Max, ?Count): Maximum element of L that has been processed
% is Max, and the number of elements in list L that are visible form left 
% is Count.  

visnum1([],_Max,1).
visnum1([L0|L1],Max,Count):-
     L0 #> Max #<=> MaxUpdate,
     MaxUpdate #=> Max1 #= L0,
     Count #= Count1 + MaxUpdate,
     #\ MaxUpdate #=> Max1 #= Max,
     visnum1(L1, Max1, Count1).

% Alternative implementation of visnum
% Assumes that all numbers are positive.	 
visibleNum(L,K):-
     visibleNum1(L,0,K).
	 
visibleNum1([],_,0).
visibleNum1([L1|L2],Prev,K):-
	L1 #> Prev #<=> Num,
	K #= Num + Num2,
	visibleNum1(L2,L1,Num2).
	 
	 
	 
% getSubgrid(+Grid, +K,?SGrid) :- SGrid is the Grid divided into subgrids of equal sizes K x K

getSubGrid([],_,[]).    
getSubGrid(Grid,K, SGrid):-
    subRows(Grid,K,NewGrid), subGrids(NewGrid,K,SGrid).

	
	
% subRows(+SGrid,+K,?SGridRow) : SGridRow is SGrid divided into subgrids 
% with K rows and same number of columns
% example : 
% subRows([[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]],2,L)
%     L = [ [[1,2,3,4],[5,6,7,8]] ,[[9,10,11,12],[13,14,15,16]] ]   

subRows([],_,[]).
subRows([SGrid0|SGrid1],K,[X1|X2]):-
    firstNElements([SGrid0|SGrid1],K,X1,Rest),subRows(Rest,K,X2).

	
	
% firstNElements(+L,+N, -L0,-L1) : L0 is the first N elements of L
% L1 is the rest of the list.
    
firstNElements([],_,[],[]).
firstNElements(L,N,L0,L1):-
    length(L0,N), append(L0,L1,L).
    
	
	
% subGrids(+SGridRow, +K, SubGrid) : Given SGridRow (As described above)
% dives each element to a subgrid with K rows and K columns.    
% example : 
% subGrids([ [[1,2,3,4],[5,6,7,8]] ,[[9,10,11,12],[13,14,15,16]] ],2,SubGrid)
% SubGrid =  [[1,2,5,6],[3,4,7,8],[9,10,13,14],[11,12,15,16]] 
    
subGrids([],_,[]).
subGrids([A|B],K,SubGrid):-
    subGrids2(A,K,SubGrid0),
    subGrids(B,K,SubGrid1),append(SubGrid0,SubGrid1,SubGrid).

	
	
% subGrids2 (+SGridElement, +K, SubGrid) : Given single element of SGridRow,
% which is a list of parsed rows, divides each element to a subgrid with 
% K rows and K columns.
% subGrids2([[1,2,3,4,5,6],[2,3,4,5,6,7]],2,Subgrid).
%   Subgrid = [[1,2,2,3],[3,4,4,5],[5,6,6,7]] 
    
subGrids2([[]|_E],_,[]).
subGrids2(L,K,[SG0|SG1]):-
    subGridSub(L,K,SG0,Rest),
    ( Rest = [[]|_End] -> SG1 = []
    ; subGrids2(Rest,K,SG1)
    ).
  
  
  
% subGridSub (+SubGridList, +K, -Grid, -Rest) : Given SubGridList,
% parses it to a single Grid of size K * K and Rest
%  subGridSub([[1,2,3,4,5,6],[2,3,4,5,6,7]],2,Subgrid,Rest).
%     Subgrid = [1,2,2,3],
%     Rest = [[3,4,5,6],[4,5,6,7]] ? 
    
subGridSub([],_,[],[]).
subGridSub([A|B], K, SubGridSub,[Rest0|Rest1]):-
    firstNElements(A, K, SubGridSub0, Rest0),
    subGridSub(B, K, SubGridSub1,Rest1),
    append(SubGridSub0,SubGridSub1,SubGridSub).

	
% mth1(?Index, ?List, ?Val)	: Val is the value of the List at index Index.
% Implemented to get rid of choice point

mth1(1,[Val|_],Val) :- !.
mth1(Index,[_|L1],Val) :-
    Index #> 1,
    Index1 #= Index-1,
    mth1(Index1,L1,Val).