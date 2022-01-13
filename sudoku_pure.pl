% to run this solution you need to run the solve predicate
% examples:
%     solve([[2, _, _, _, 8, _, 3, _, _],
%           [_, 6, _, _, 7, _, _, 8, 4],
%           [_, 3, _, 5, _, _, 2, _, 9],
%           [_, _, _, 1, _, 5, 4, _, 8],
%           [_, _, _, _, _, _, _, _, _],
%           [4, _, 2, 7, _, 6, _, _, _],
%           [3, _, 1, _, _, 7, _, 4, _],
%           [7, 2, _, _, 4, _, _, 6, _],
%           [_, _, 4, _, 1, _, _, _, 3]], 3).
%
%     solve([[1, _, _, 9, 2, _, _, _, _],
%           [5, 2, 4, _, 1, _, _, _, _],
%           [_, _, _, _, _, _, _, 7, _],
%           [_, 5, _, _, _, 8, 1, _, 2],
%           [_, _, _, _, _, _, _, _, _],
%           [4, _, 2, 7, _, _, _, 9, _],
%           [_, 6, _, _, _, _, _, _, _],
%           [_, _, _, _, 3, _, 9, 4, 5],
%           [_, _, _, _, 7, 1, _, _, 6]], 3).
%
%     solve([[_, _, _, 9, _, _, _, _, 2],
%           [_, 5, _, 1, 2, 3, 4, _, _],
%           [_, 3, _, _, _, _, 1, 6, _],
%           [9, _, 8, _, _, _, _, _, _],
%           [_, 7, _, _, _, _, _, 9, _],
%           [_, _, _, _, _, _, 2, _, 5],
%           [_, 9, 1, _, _, _, _, 5, _],
%           [_, _, 7, 4, 3, 9, _, 2, _],
%           [4, _, _, _, _, 7, _, _, _]], 3).
%
% press ';' to cicle through all possible solutions

% solve: main predicate, solves the sudoku puzzle
solve(X, N) :- solve(X, N, D), print(D).
solve(X, N, D) :- get_squares(X, N, XS), transpose(X, XT),
    domain(N, D1, X, XT, XS),
    (goal(D1), !, D1 = D; D1 == X, !, replace_element(D1, N, DR), solve(DR, N, D), !; solve(D1, N, D)).

% goal: predicate that checks if the soduko is solved
goal([]).
goal([[]|X]) :- goal(X).
goal([[E|R]|X]) :- number(E), goal([R|X]).

% replace_element: no domain can be reduced so a value must be replaced
replace_element(D, N, DR) :- NN is N*N, length(DR, NN), replace_element(D, N, NN, DR, 0, 0, false).
replace_element(D, N, NN, DR, R, C, RP) :- R<NN, C<NN, NC is C + 1, nth0(R, D, Row), nth0(C, Row, X), nth0(R, DR, DRow), length(DRow, NN), nth0(C, DRow, DX), (RP, !, X = DX, NRP = true; number(X), !, X = DX, NRP = false; member(DX, X), NRP = true), replace_element(D, N, NN, DR, R, NC, NRP).
replace_element(D, N, NN, DR, R, NN, RP) :- NR is R + 1, replace_element(D, N, NN, DR, NR, 0, RP).
replace_element(_, _, NN, _, NN, _, _).

% domain: predicate that finds the domain of all cells of N²*N² sudoku.
% Given sudoku Rows, columns, squares the domain can be restricted, for example for the line _, _, _, 2: the value two can be avoided
domain(N, Domains, Rows, Columns, Squares) :- NN is N*N, length(Domains, NN), numlist(1, NN, DigitList), domain(N, NN, Domains, Rows, Columns, Squares, DigitList, 0, 0).
domain(N, NN, Domains, Rows, Columns, Squares, DigitList, R, C):- nth0(R, Rows, Row), nth0(C, Row, X), number(X), !, nth0(R, Domains, DR), length(DR, NN), nth0(C, DR, X),
NC is C + 1, domain(N, NN, Domains, Rows, Columns, Squares, DigitList, R, NC).
domain(N, NN, Domains, Rows, Columns, Squares, DigitList, R, C) :- NC is C + 1, R<NN, C<NN, nth0(R, Rows, Row), nth0(C, Columns, Column), S is (R div N)*N+(C div N), nth0(S, Squares, Square),
    numbers(Row, NRow), numbers(Column, NColumn), numbers(Square, NSquare), subtract(DigitList, NRow, RO), subtract(RO, NColumn, RC), subtract(RC, NSquare, RCS),
    nth0(R, Domains, DR), length(DR, NN), (RCS = [SOL]; RCS = [], false ; RCS = [_, _ |_], SOL = RCS), nth0(C, DR, SOL),
    domain(N, NN, Domains, Rows, Columns, Squares, DigitList, R, NC).
domain(N, NN, Domains, Rows, Columns, Squares, DigitList, R, NN) :- NR is R + 1, domain(N, NN, Domains, Rows, Columns, Squares, DigitList, NR, 0).
domain(_, NN, _, _, _, _, _, NN, _).

% numbers: get all numbers in list, variables and lists are ignored
numbers([], []).
numbers([N|L], [N|Numbers]) :- number(N), numbers(L, Numbers).
numbers([V|L], Numbers) :- \+ number(V), numbers(L, Numbers).

% first_column: predicate that gets the first column of matrice and the rest of it.
% this predicate is useful when transposing the matrice
first_column([], [], []).
first_column([[E|L]|X], [E|C], [L|LX]) :- first_column(X, C, LX).

% transpose: predicate that calculates the predicate of a matrice
transpose(X, []) :- maplist(=([]), X).
transpose(X, [C|XT]) :- first_column(X, C, R), transpose(R, XT).

% generate_empty: predicate that generate N empty lists
generate_empty(N, E) :- length(E, N), maplist(=([]), E).

% get_squares(X, N, XS, Bag, Index).
% construct the squares from the matrice
get_squares(X, N, XS):- generate_empty(N, E), get_squares(X, N, XS, E, 1).
get_squares([], N, [], _, NN):- NN is N*N+1.
get_squares([L|X], N, XS, Bag, Index) :- 0 is Index mod N, temp(L, N, Bag, Nbag), append(Nbag, TS, XS), Nindex is Index + 1, generate_empty(N, E), get_squares(X, N, TS, E, Nindex).
get_squares([L|X], N, XS, Bag, Index) :- \+ 0 is Index mod N, temp(L, N, Bag, Nbag), Nindex is Index + 1, get_squares(X, N, XS, Nbag, Nindex).

% temp
temp([], _, [], []).
temp(L, N, [PS|Bag], [S|Nbag]) :- length(SS, N), append(SS, NL, L), append(PS, SS, S), temp(NL, N, Bag, Nbag).

% print the sudoku
print([]).
print([H|T]) :-
  write(H),nl,
  print(T).