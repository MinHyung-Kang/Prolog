% consistent(+SGrid) : For all areas of the Sudoku grid SGrid it holds
% that all positive integers in the area are distinct.

consistent([]).
consistent(SGrid):-
    consistentRows(SGrid), consistentColumns(SGrid), consistentSubGrid(SGrid).

consistent2([]).
consistent2(SGrid):-
    consistentRows(SGrid), consistentColumns(SGrid).

% consistentRows(+SGrid) :- For all the rows of the sudoku grid SGrid it holds
% that all positive integers are distinct.

consistentRows([]).
consistentRows([A|B]):-
    noDuplicateElement(A), consistentRows(B).

% load clpfd library to use transpose    
%:- use_module(library(clpfd),[]).
:- use_module(library(lists)).
    
% consistentColumns(+SGrid) :- For all the columns of the sudoku grid all the
% positive integers are distinct.

consistentColumns([]).
consistentColumns(SGrid):-
  transpose(SGrid, SGridT), consistentRows(SGridT).


% consistentSubgrid(+SGrid) :- For all the subgrids of the sudoku grid all the
% positive integers are distinct.

consistentSubGrid([]).    
consistentSubGrid(SGrid):-
    getDimensions(SGrid,_L,K), subRows(SGrid,K,NewGrid), subGrids(NewGrid,K,SquareGrid),
    consistentRows(SquareGrid).

% subRows(+SGrid,+K,-SGridRow) : SGridRow is SGrid divided into subgrids 
% with K rows and same number of columns
%
% nsubRows([[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]],2,L)
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
% subGrids([ [[1,2,3,4],[5,6,7,8]] ,[[9,10,11,12],[13,14,15,16]] ],2,SubGrid)
%  SubGrid =  [[[[1,2],[5,6]],[[3,4],[7,8]]],
%              [[[9,10],[13,14]],[[11,12],[15,16]]]] .
% To test :  subRows([[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]],2,L),subGrids(L,2,SubGrid).
    
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
    
% getDimensions(+SGrid, -L, -K) : returns the length of SGrid L,
% and size of SGrid K

getDimensions(SGrid, L, K):-
    length(SGrid,L), K is integer(sqrt(L)).

% noDuplicateElements(+L) : A given list has no repeating integer besides 0

noDuplicateElement([]).
noDuplicateElement([A|B]):-
    (   A = 0 -> noDuplicateElement(B)
    ;   noDuplicateElement(B,A), noDuplicateElement(B)
    ).

    
% noDuplicateElement(+L,+E) : List L has no occurence of E
    
noDuplicateElement([],_).
noDuplicateElement([A|B],E):-
    (   E = A -> 1 = 2
    ;   noDuplicateElement(B,E)
    ).
