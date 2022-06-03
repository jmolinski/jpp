
:- use_module(library(lists)).

% ---------------------------------------------------------------------------

initBST([N|Ns], T0, T) :-
    insertBST(N, T0, T1),
    initBST(Ns, T1, T).
initBST([], T, T).

findBST(X, t(X, _, _)).
findBST(X, t(V, L, _)) :- 
    X < V, findBST(X, L).
findBST(X, t(V, _, R)) :- 
    X > V, findBST(X, R).

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

deleteBST(V, t(V, nil, R), R).
deleteBST(V, t(V, L, nil), L).
deleteBST(V, t(NVal, nil, nil), t(NVal, nil, nil)) :- 
    V \= NVal.
deleteBST(V, t(NVal, L, R), t(NVal, L, DRight)) :- 
    V > NVal, deleteBST(V, R, DRight).
deleteBST(V,t(NVal,L,R),t(NVal, DLeft, R)) :- 
    V < NVal, deleteBST(V, L, DLeft).
deleteBST(V, t(V, L, R), t(NewVal, L, DRight)) :- 
    getLeftBST(R, NewVal), deleteBST(NewVal, R, DRight).

getLeftBST(t(V, nil, _), V).
getLeftBST(t(_, L, _), NVal) :- 
    getLeftBST(L, NVal).

% -----------------------------------------------------------------------------

% `dfa(FunkcjaPrzejścia, StanPoczątkowy, ZbiórStanówAkceptujących)`

% T  - funkcja przejścia
% Q0 - stan początkowy
% F  - zbiór stanów akceptujących

% przejście: fp(S1, C, S2)


% correct(+Automat, -Reprezentacja)
correct(dfa(T, Q0, F), R) :-
    alphabetFromTF(T, Alphabet),
    statesFromTF(T, States),
    lists.head(Alphabet, _),          % Czy alfabet jest niepusty?
    lists.member(Q0, States),         % Czy stan początkowy jest w zbiorze stanów?
    % Czy wszystkie stany akceptujące są w zbiorze stanów?
    lists.subtract(F, States, []).






% 1. funkcja jest całkowita (z każdego stanu wychodzi każda litera)
% 2. stanPoczatkowy jest stanem
% 3. stanyAkceptujace sa stanami
% 4. alfabet niepusty
% 5. zbior stanow niepusty
% 6. liczba przejść == liczba stanów razy liczba liter

