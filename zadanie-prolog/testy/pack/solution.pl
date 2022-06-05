example(a11, dfa([fp(1,a,1),fp(1,b,2),fp(2,a,2),fp(2,b,1)], 1, [2,1])).
example(a12, dfa([fp(x,a,y),fp(x,b,x),fp(y,a,x),fp(y,b,x)], x, [x,y])).
example(a2, dfa([fp(1,a,2),fp(2,b,1),fp(1,b,3),fp(2,a,3),fp(3,b,3),fp(3,a,3)], 1, [1])).
example(a3, dfa([fp(0,a,1),fp(1,a,0)], 0, [0])).
example(a4, dfa([fp(x,a,y),fp(y,a,z),fp(z,a,x)], x, [x])).
example(a5, dfa([fp(x,a,y),fp(y,a,z),fp(z,a,zz),fp(zz,a,x)], x, [x])).
example(a6, dfa([fp(1,a,1),fp(1,b,2),fp(2,a,2),fp(2,b,1)], 1, [])).
example(a7, dfa([fp(1,a,1),fp(1,b,2),fp(2,a,2),fp(2,b,1),fp(3,b,3),fp(3,a,3)], 1, [3])).
example(b1, dfa([fp(1,a,1),fp(1,a,1)], 1, [])).
example(b2, dfa([fp(1,a,1),fp(1,a,2)], 1, [])).
example(b3, dfa([fp(1,a,2)], 1, [])).
example(b4, dfa([fp(1,a,1)], 2, [])).
example(b5, dfa([fp(1,a,1)], 1, [1,2])).
example(b6, dfa([], [], [])).

% ------------------------------------------------------------------------------

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


% -----------------------------------------------------------------------------

% `dfa(FunkcjaPrzejścia, StanPoczątkowy, ZbiórStanówAkceptujących)`

% T  - funkcja przejścia = TF
% Q0 - stan początkowy
% F  - zbiór stanów akceptujących

% przejście: fp(S1, C, S2)

head([H|_], H).

allMembers([], _).
allMembers([H|T], X) :- 
    member(H, X), allMembers(T, X).

alphabetFromTF([], []).
alphabetFromTF([fp(_, C, _)|FunTail], [C|AlphabetTail]) :- 
    alphabetFromTF(FunTail, AlphabetTail).

statesFromTF([], []).
statesFromTF([fp(S1, _, S2)|FunTail], [S1, S2|StatesTail]) :- 
    statesFromTF(FunTail, StatesTail).

areListsEqualLength(L1, L2) :-
    length(L1, S1),
    length(L2, S2),
    S1 = S2.

% outgoingTransitionsFromState(+TF, +State, -Outgoing).
outgoingTransitionsFromState([], _, []).
outgoingTransitionsFromState([fp(State, C, _)|TFunTail], State, [C|OutgoingTail]) :- 
    outgoingTransitionsFromState(TFunTail, State, OutgoingTail).
outgoingTransitionsFromState([_|TFunTail], State, OutgoingTail) :- 
    outgoingTransitionsFromState(TFunTail, State, OutgoingTail).

% outgoingTransitionsFromState(+TF, +State, +Alphabet).
stateHasAllTransitions(TF, State, Alphabet) :-
    outgoingTransitionsFromState(TF, State, Outgoing),
    areListsEqualLength(Outgoing, Alphabet),
    allMembers(Outgoing, Alphabet).

% isFunctionComplete(+States, +Alphabet, +TF).
isFunctionComplete([], _, _).
isFunctionComplete([S|StatesTail], Alphabet, TF) :-
    stateHasAllTransitions(TF, S, Alphabet),
    isFunctionComplete(StatesTail, Alphabet, TF).

% -----------------------------------------------------------------------------
%                                  correct 
% -----------------------------------------------------------------------------

% tfToBST(+TF, -TransitionsBST)
tfToBST([], nil).
tfToBST([fp(S1, C, S2) | TFTail], TransitionsBST) :-
    tfToBST(TFTail, TransitionsBSTTail),
    insertBST(kv(k(S1, C), S2), TransitionsBSTTail, TransitionsBST).

% dfaRepresentation(+TF, +Q0, +F, +Alphabet, +States, -Representation).
dfaRepresentation(TF, Q0, F, Alphabet, States, R) :-
    initBST(F, nil, AcceptingStatesBST),
    tfToBST(TF, TFBST),
    R = dfaRepr(Q0, F, TF, AcceptingStatesBST, TFBST, x, x, x).


% correct(+Automat, -Reprezentacja)
correct(dfa(TF, Q0, F), Representation) :-
    ground(dfa(TF, Q0, F)),
    alphabetFromTF(TF, AlphabetWithDuplicates),
    sort(AlphabetWithDuplicates, Alphabet),
    statesFromTF(TF, StatesWithDuplicates),
    sort(StatesWithDuplicates, States),
    % Czy w zbiorze F nie ma powtórzeń?
    %sort(F, AcceptingStates),
    %areListsEqualLength(F, AcceptingStates),
    % Czy alfabet jest niepusty?
    %head(Alphabet, _),
    % Czy stan początkowy jest w zbiorze stanów?
    %member(Q0, States),         
    % Czy wszystkie stany akceptujące są w zbiorze stanów?
    %allMembers(F, States),
    % Czy funkcja przejścia moze być funkcją? liczba przejść == liczba stanów razy liczba liter TODO
    length(TF, TFLength),
    length(States, StatesLength),
    length(Alphabet, AlphabetLength),
    TFLength is StatesLength * AlphabetLength,
    % Czy funkcja przejścia jest całkowita?
    isFunctionComplete(States, TF, Alphabet),
    dfaRepresentation(TF, Q0, F, Alphabet, States, Representation).

% -----------------------------------------------------------------------------
%                                  accept 
% -----------------------------------------------------------------------------

accept(State, dfaRepr(_, _, _, AcceptingStatesBST, _, _, _, _), []) :-
    memberBST(State, AcceptingStatesBST).

accept(State, dfaRepr(_, _, _, _, TransitionsBST, _, _, _), [Letter|Tail]) :-
    findBST(k(State, Letter), TransitionsBST, NextState),
    accept(NextState, repr(_, _, TransitionsBST), Tail).

% accept(+Automat, ?Słowo)
accept(A, S) :- 
    correct(A, DFA),
    DFA = dfaRepr(Q0, _, _, _, _, _, _, _),
    accept(Q0, DFA, S).

% -----------------------------------------------------------------------------
%                                  empty 
% -----------------------------------------------------------------------------

% empty(+Automat)
empty(A) :- % TODO
    correct(A, DFA). 

% -----------------------------------------------------------------------------
%                                  part 2
% -----------------------------------------------------------------------------

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


