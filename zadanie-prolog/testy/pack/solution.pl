
:- use_module(library(lists)).

% ---------------------------------------------------------------------------

findBST(Key, t(kv(Key, Value), _, _), Value).
findBST(Key, t(kv(NodeKey, _), L, _), _) :- 
    Key @< NodeKey, findBST(Key, L, _).
findBST(X, t(NodeKey, _, R), _) :- 
    Key @> NodeKey, findBST(Key, R, _).

memberBST(X, t(X, _, _)).
memberBST(X, t(V, L, _)) :- 
    X @< V, memberBST(X, L).
memberBST(X, t(V, _, R)) :- 
    X @> V, memberBST(X, R).

insertBST(I, nil, t(I, nil, nil)).
insertBST(I, t(X, L, R), t(Y, P, Q)) :-
    (   I < X
    ->  insertBST(I, L, U),
        (P, Y, Q) = (U, X, R)
    ;   I > X
    ->  insertBST(I, R, U),
        (P, Y, Q) = (L, X, U)
    ;   (P, Y, Q) = (L, X, R)  
    ).

initBST([], T, T).
initBST([H|Tail], T0, T) :-
    insertBST(H, T0, T1),
    initBST(Tail, T1, T).

%deleteBST(V, t(V, nil, R), R).
%deleteBST(V, t(V, L, nil), L).
%deleteBST(V, t(NVal, nil, nil), t(NVal, nil, nil)) :- 
%    V \= NVal.
%deleteBST(V, t(NVal, L, R), t(NVal, L, DRight)) :- 
%    V > NVal, deleteBST(V, R, DRight).
%deleteBST(V,t(NVal,L,R),t(NVal, DLeft, R)) :- 
%    V < NVal, deleteBST(V, L, DLeft).
%deleteBST(V, t(V, L, R), t(NewVal, L, DRight)) :- 
%    getLeftBST(R, NewVal), deleteBST(NewVal, R, DRight).

getLeftBST(t(V, nil, _), V).
getLeftBST(t(_, L, _), NVal) :- 
    getLeftBST(L, NVal).

% -----------------------------------------------------------------------------

% `dfa(FunkcjaPrzejścia, StanPoczątkowy, ZbiórStanówAkceptujących)`

% T  - funkcja przejścia = TF
% Q0 - stan początkowy
% F  - zbiór stanów akceptujących

% przejście: fp(S1, C, S2)

allMembers([], _).
allMembers([H|T], X) :- 
    member(H, X), allMembers(T, X).

alphabetFromTF([], []).
alphabetFromTF([fp(_, C, _)|FunTail], [C|AlphabetTail]) :- 
    alphabetFromTF(FunTail, AlphabetTail).

statesFromTF([], []).
statesFromTF([fp(S1, _, S2)|FunTail], [S1, S2|StatesTail]) :- 
    statesFromTF(FunTail, StatesTail).

listUnique([], []).
listUnique([Head | Tail], Result) :-
    member(Head, Tail),
    listUnique(Tail, Result).
listUnique([Head | Tail], [Head | Result]) :-
    listUnique(Tail, Result).

isFunctionComplete(TF, Alphabet, States). % TODO

dfaRepresentation(TF, Q0, F, Alphabet, States, repr(Q0, F, TF)). % TODO

head([H|_], H).

% correct(+Automat, -Reprezentacja)
correct(dfa(T, Q0, F), Representation) :-
    alphabetFromTF(T, Alphabet),
    statesFromTF(T, StatesWithDuplicates),
    listUnique(StatesWithDuplicates, States),
    % Czy alfabet jest niepusty?
    head(Alphabet, _),          % TODO import module lists 
    % Czy stan początkowy jest w zbiorze stanów?
    member(Q0, States),         
    % Czy wszystkie stany akceptujące są w zbiorze stanów?
    allMembers(F, States),
    % Czy funkcja przejścia jest całkowita?
    isFunctionComplete(T, Alphabet, States),
    dfaRepresentation(T, Q0, F, Alphabet, States, Representation).


accept(State, repr(_, AcceptingStatesBST, _), []) :-
    memberBST(State, AcceptingStatesBST).

accept(State, repr(_, _, TransitionsBST), [Letter|Tail]) :-
    findBST(k(State, Letter), TransitionsBST, NextState),
    accept(NextState, repr(_, _, TransitionsBST), Tail).

% accept(+Automat, ?Słowo)
accept(A, S) :- 
    correct(A, DFA),
    DFA = repr(Q0, _, _),
    accept(Q0, DFA, S).

% empty(+Automat)
empty(A) :- % TODO
    correct(A, DFA). 

% equal(+Automat1, +Automat2)
equal(A1, A2) :-
    subsetEq(A1, A2),
    subsetEq(A2, A1). 

complementDFA(A, Complement). % TODO
intersectDFA(A, B, Intersection). % TODO

% subsetEq(+Automat1, +Automat2)
subsetEq(A1, A2) :-
    correct(A1, DFA1),
    correct(A2, DFA2),
    complementDFA(DFA2, DFA2Complement),
    intersectDFA(DFA1, DFA2Complement, Intersection),
    empty(Intersection).


% 1. funkcja jest całkowita (z każdego stanu wychodzi każda litera)
% 2. stanPoczatkowy jest stanem
% 3. stanyAkceptujace sa stanami
% 4. alfabet niepusty
% 5. zbior stanow niepusty
% 6. liczba przejść == liczba stanów razy liczba liter

