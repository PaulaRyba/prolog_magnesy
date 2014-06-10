% Paula Ryba, nr indeksu 240347, magnesy
%
% Rozwiązania dla testów z KNO:
% magnesy2: 
% 621 inferences, 0.000 CPU in 0.001 seconds (0% CPU, Infinite Lips)
% S = [[dN, d, d, d], [uS, u, u, u], [d, dN, d, d], [u, uS, u, u]] ;
% 426 inferences, 0.000 CPU in 0.000 seconds (?% CPU, Infinite Lips)
% S = [[d, dN, d, d], [u, uS, u, u], [dN, d, d, d], [uS, u, u, u]] ;
% 20 inferences, 0.000 CPU in 0.000 seconds (?% CPU, Infinite Lips)
% false.
%
% magnesy3:
% 598 inferences, 0.000 CPU in 0.001 seconds (0% CPU, Infinite Lips)
% S = [[dN, r, l, dN], [uS, dN, d, uS], [d, uS, u, d], [u, rN, lS, u]] ;
% 230 inferences, 0.000 CPU in 0.000 seconds (?% CPU, Infinite Lips)
% false.

% Rozwiązanie dla testu z wielkością planszy 7x4:
% 3,698 inferences, 0.000 CPU in 0.001 seconds (0% CPU, Infinite Lips)
% X = [[rN, lS, d, rN, lS, r, l], [dS, d, u, r, l, d, dN], [uN, u, d, r, l, u|...], [rS, lN, u, rS, lN|...]] ;
% 4,256 inferences, 0.000 CPU in 0.002 seconds (0% CPU, Infinite Lips)
% X = [[rN, lS, d, rS, lN, r, l], [dS, d, u, r, l, d, dN], [uN, u, d, r, l, u|...], [rS, lN, u, rN, lS|...]] ;
% 14,830 inferences, 0.000 CPU in 0.005 seconds (0% CPU, Infinite Lips)
% X = [[rS, lN, d, rN, lS, r, l], [dN, d, u, r, l, d, dS], [uS, u, d, r, l, u|...], [rN, lS, u, rS, lN|...]] ;
% 4,256 inferences, 0.000 CPU in 0.001 seconds (0% CPU, Infinite Lips)
% X = [[rS, lN, d, rS, lN, r, l], [dN, d, u, r, l, d, dS], [uS, u, d, r, l, u|...], [rN, lS, u, rN, lS|...]] ;
% 16,088 inferences, 0.000 CPU in 0.005 seconds (0% CPU, Infinite Lips)
% false.

% Kolejność wczytywania jest dobra dla podanych na KNO testów (odwrotnie niż w poleceniu zadania). 
% W przypadku niektórych innych testów (z ii.yebood.com) należy zamienić kolejność wczytywania
% wierszy i kolumn (linie 41 i 42 oraz 43 i 44)


magnesy(File,Sol3):-
 
 open(File, read, _, [alias(params)]),
  read(params, _),
  read(params, _),
  read(params, Tab),
  read(params, ColNorth),
  read(params, RowNorth),
  read(params, ColSouth),
  read(params, RowSouth),
  close(params),
  solveR(Tab,RowNorth,ColNorth,RowSouth,ColSouth,Sol, NewColNorth, NewColSouth, []),
  rotates(Sol,So),
  solveC(So, NewColNorth, RowNorth, NewColSouth, RowSouth, Sol2, []),
  checkTab(Sol2),
  rotates(Sol2, Sol3),
  checkTab(Sol3),
  checkLines(Sol3, RowNorth, RowSouth).

% rozwiązuje wiersze
solveR([T|Tab],[A1|RowNorth],ColNorth,[C1|RowSouth],ColSouth,[S|Sol], X, Y, LastRow):-
  genR(T,A1,ColNorth,ColNAcc,C1,ColSouth,ColSAcc,S, 0, r),
  checkTwoRows(S, LastRow),
  solveR(Tab,RowNorth,ColNAcc,RowSouth,ColSAcc,Sol, X, Y, S).
solveR([],[],X,[],Y,[], X, Y, _).

% sprawdza dwa wiersze naraz
checkTwoRows(_, []).
checkTwoRows([H1|T1], [H2|T2]) :-
  ( (north(H1), notNorth(H2)) ; (south(H1),  notSouth(H2)) ; (notNorth(H1), notSouth(H1)) ), !, 
  checkTwoRows(T1, T2).

% rozwiązuje kolumny
solveC([T|Tab],[A1|RowNorth],ColNorth,[C1|RowSouth],ColSouth,[S|Sol], LastRow):-
  genC(T,A1,ColNorth,ColNAcc,C1,ColSouth,ColSAcc,S, d),
  checkTwoRows(S, LastRow),
  solveC(Tab,RowNorth,ColNAcc,RowSouth,ColSAcc,Sol, S).
solveC([],[],_,[],_,[], _).


% generuje pojedynczy wiersz
genR([T|Tab], RowNorth, [ColNorth1|ColNTail], [ColNorth1|ColNAcc], RowSouth, [ColSouth1|ColSTail], [ColSouth1|ColSAcc], [T|S], Skipped, _) :-
  (T = u ; T = uN ; T = uS ; T = d ; T = dS ; T = dN), !, 
  Skipped2 is Skipped + 1,
  genR(Tab, RowNorth, ColNTail, ColNAcc, RowSouth, ColSTail, ColSAcc, S, Skipped2, T).

% uzupełnia magnes rN-lS
genR([X1, X2|T],RowNorth,[ColNorth1, ColNorth2|ColNTail],[B1, ColNorth2|ColNAcc],RowSouth,[ColSouth1, ColSouth2|ColSTail],[ColSouth1, D2|ColSAcc],[NL, NR|S], Skipped, Last):-
  notNorth(Last),
  RowNorth>0, A1 is RowNorth-1, 
  RowSouth>0, A2 is RowSouth-1,
  ColNorth1>0, B1 is ColNorth1-1, 
  ColSouth2>0, D2 is ColSouth2-1,
  magnesrNlS(X1, X2, NL, NR),
  genR(T,A1,ColNTail,ColNAcc,A2,ColSTail,ColSAcc,S, Skipped, lS).

% uzupełnia magnes rS-lN
genR([X1, X2|T],RowNorth,[ColNorth1, ColNorth2|ColNTail],[ColNorth1, B2|ColNAcc],RowSouth,[ColSouth1, ColSouth2|ColSTail],[D1, ColSouth2|ColSAcc],[NL, NR|S], Skipped, Last):-
  notSouth(Last),
  RowNorth>0, A1 is RowNorth-1, 
  RowSouth>0, A2 is RowSouth-1,
  ColNorth2>0, B2 is ColNorth2-1,
  ColSouth1>0, D1 is ColSouth1-1, 
  magnesrSlN(X1, X2, NL, NR),
  genR(T,A1,ColNTail,ColNAcc,A2,ColSTail,ColSAcc,S, Skipped, lN).

% uzupełnia magnes r-l
genR([X1, X2|T],RowNorth,[ColNorth1, ColNorth2|ColNTail],[ColNorth1, ColNorth2|ColNAcc],RowSouth,[ColSouth1, ColSouth2|ColSTail],[ColSouth1, ColSouth2|ColSAcc],[NL, NR|S], Skipped, _):-
  magnesrl(X1, X2, NL, NR),
  genR(T,RowNorth,ColNTail,ColNAcc,RowSouth,ColSTail,ColSAcc,S, Skipped, r).

genR([], RowNorth, _, [], RowSouth, _, [], [], Skipped, _) :-
  RowNorth =< Skipped,
  RowSouth =< Skipped.

% definicje magnesów
magnesrNlS(R, _, RN, LN) :- R = rN, !, RN = rN, LN = lS.
magnesrNlS(_, L, RN, LN) :- L = lS, !, RN = rN, LN = lS.
magnesrNlS(R, L, RN, LN) :- R = r, L = l, RN = rN, LN = lS.
magnesrSlN(R, _, RN, LN) :- R = rS, !, RN = rS, LN = lN.
magnesrSlN(_, L, RN, LN) :- L = lS, !, RN = rS, LN = lN.
magnesrSlN(R, L, RN, LN) :- R = r, L = l, RN = rS, LN = lN.
magnesrl(r, l, r, l).

% generuje kolumny
genC([T|Tab], RowNorth, [ColNorth1|ColNTail], [ColNorth1|ColNAcc], RowSouth, [ColSouth1|ColSTail], [ColSouth1|ColSAcc], [T|S], _) :-
  (T = r ; T = rN ; T = rS ; T = l ; T = lS ; T = lN), !, 
  genC(Tab, RowNorth, ColNTail, ColNAcc, RowSouth, ColSTail, ColSAcc, S, T).

% uzupełnia magnes dN-uS
genC([X1, X2|T],RowNorth,[ColNorth1, ColNorth2|ColNTail],[B1, ColNorth2|ColNAcc],RowSouth,[ColSouth1, ColSouth2|ColSTail],[ColSouth1, D2|ColSAcc],[NL, NR|S], Last):-
  notNorth(Last),
  RowNorth>0, A1 is RowNorth-1, 
  RowSouth>0, A2 is RowSouth-1,
  ColNorth1>0, B1 is ColNorth1-1, 
  ColSouth2>0, D2 is ColSouth2-1,
  magnesdNuS(X1, X2, NL, NR),
  genC(T,A1,ColNTail,ColNAcc,A2,ColSTail,ColSAcc,S, uS).

% uzupełnia magnes dS-uN
genC([X1, X2|T],RowNorth,[ColNorth1, ColNorth2|ColNTail],[ColNorth1, B2|ColNAcc],RowSouth,[ColSouth1, ColSouth2|ColSTail],[D1, ColSouth2|ColSAcc],[NL, NR|S], Last):-
  notSouth(Last),
  RowNorth>0, A1 is RowNorth-1, 
  RowSouth>0, A2 is RowSouth-1,
  ColNorth2>0, B2 is ColNorth2-1,
  ColSouth1>0, D1 is ColSouth1-1, 
  magnesdSuN(X1, X2, NL, NR),
  genC(T,A1,ColNTail,ColNAcc,A2,ColSTail,ColSAcc,S, uN).

% uzupełnia magnes d-u
genC([X1, X2|T],RowNorth,[ColNorth1, ColNorth2|ColNTail],[ColNorth1, ColNorth2|ColNAcc],RowSouth,[ColSouth1, ColSouth2|ColSTail],[ColSouth1, ColSouth2|ColSAcc],[NL, NR|S], _):-
  magnesdu(X1, X2, NL, NR),
  genC(T,RowNorth,ColNTail,ColNAcc,RowSouth,ColSTail,ColSAcc,S, r).

genC([], 0, _, _, 0, _, _, [], _).

magnesdNuS(D, _, DN, UN) :- D = dN, !, DN = dN, UN = uS.
magnesdNuS(_, U, DN, UN) :- U = uS, !, DN = dN, UN = uS.
magnesdNuS(D, U, DN, UN) :- D = d, U = u, DN = dN, UN = uS.
magnesdSuN(D, _, DN, UN) :- D = dS, !, DN = dS, UN = uN.
magnesdSuN(_, U, DN, UN) :- U = uS, !, DN = dS, UN = uN.
magnesdSuN(D, U, DN, UN) :- D = d, U = u, DN = dS, UN = uN.
magnesdu(d, u, d, u).

% sprawdza planszę pod względem liczby magnesów o poszczególnych biegunach
checkLines([], [], []).
checkLines([H|T], [NH|NT], [SH|ST]) :-
  checkLine(H, NH, SH),
  checkLines(T, NT, ST).

checkLine([], 0, 0).
checkLine([P|T], RowNorth, RowSouth) :-
  north(P), !, 
  NewRowNorth is RowNorth - 1, 
  checkLine(T, NewRowNorth, RowSouth).
checkLine([P|T], RowNorth, RowSouth) :-
  south(P), !, 
  NewRowSouth is RowSouth - 1, 
  checkLine(T, RowNorth, NewRowSouth).
checkLine([_|T], RowNorth, RowSouth) :-
  checkLine(T, RowNorth, RowSouth).

north(rN) :- !.
north(lN) :- !.
north(dN) :- !.
north(uN) :- !.
south(rS) :- !.
south(lS) :- !.
south(dS) :- !.
south(uS) :- !.

notNorth(X) :-
  south(X), !.
notNorth(l) :- !.
notNorth(r) :- !.
notNorth(d) :- !.
notNorth(u) :- !.

notSouth(X) :-
  north(X), !.
notSouth(l) :- !.
notSouth(r) :- !.
notSouth(d) :- !.
notSouth(u) :- !.

% sprawdza tablicę pod względem kolizji biegunów
checkTab([]) :- !.
checkTab([Row|T]) :-
  check(Row),
  checkTab(T).

check([]) :- !.
check([_]) :- ! .
check([H1, H2|T]) :-
  ( (north(H1), notNorth(H2)) ; (south(H1),  notSouth(H2)) ; (notNorth(H1), notSouth(H1)) ), !,  
  check([H2|T]).  

head([],[]).
head([X|_],X).

tail([_|Xs],Xs):-!. 
tail(_,[]).

% transponuje planszę
rot([],[],[]).
rot([X|Xs],[H|Hs],[T|Ts]):-
  head(X,H),
  tail(X,T),
  rot(Xs,Hs,Ts).

rotates(L,[]):-islistofemptylists(L),!.
rotates(X,[W|Ws]):-
  rot(X,W,T),!,
  rotates(T,Ws).

islistofemptylists([]):-!.
islistofemptylists([W|Ws]):-W=[], islistofemptylists(Ws).
