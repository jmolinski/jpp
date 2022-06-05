


% -----------------------------------------------------------------------------
%                                    BST 
% -----------------------------------------------------------------------------

findBST(Key, t(kv(Key, Value), _, _), Value).
findBST(Key, t(kv(NodeKey, _), L, _), Value) :- 
    Key @< NodeKey, findBST(Key, L, Value).
findBST(Key, t(kv(NodeKey, _), _, R), Value) :- 
    Key @> NodeKey, findBST(Key, R, Value).

memberBST(X, t(X, _, _)).
memberBST(X, t(V, L, _)) :- 
    X @< V, memberBST(X, L).
memberBST(X, t(V, _, R)) :- 
    X @> V, memberBST(X, R).

insertBST(I, nil, t(I, nil, nil)).
insertBST(I, t(X, L, R), t(Y, P, Q)) :-
    (   I @< X
    ->  insertBST(I, L, U),
        (P, Y, Q) = (U, X, R)
    ;   I @> X
    ->  insertBST(I, R, U),
        (P, Y, Q) = (L, X, U)
    ;   (P, Y, Q) = (L, X, R)  
    ).

initBST([], T, T).
initBST([H|Tail], T0, T) :-
    insertBST(H, T0, T1),
    initBST(Tail, T1, T).

% -----------------------------------------------------------------------------
%                                  correct 
% -----------------------------------------------------------------------------

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

% isFunctionComplete(+TF, +Alphabet, +States).
isPossiblyFunction(TF, Alphabet, States) :-
    length(TF, TFLength),
    length(States, StatesLength),
    length(Alphabet, AlphabetLength),
    TFLength =:= StatesLength * AlphabetLength.


% tfToBST(+TF, -TransitionsBST)
tfToBST([], nil).
tfToBST([fp(S1, C, S2) | TFTail], TransitionsBST) :-
    tfToBST(TFTail, TransitionsBSTTail),
    insertBST(kv(k(S1, C), S2), TransitionsBSTTail, TransitionsBST).

% dfaRepresentation(+TF, +Q0, +F, +Alphabet, +States, -Representation).
dfaRepresentation(TF, Q0, F, Alphabet, States, R) :-
    initBST(F, nil, AcceptingStatesBST),
    tfToBST(TF, TFBST),
    R = dfaRepr(Q0, F, AcceptingStatesBST, TFBST, Alphabet, x, States).


% correct(+Automat, -Reprezentacja)
correct(dfa(TF, Q0, F), Representation) :-
    ground(dfa(TF, Q0, F)),
    alphabetFromTF(TF, AlphabetWithDuplicates),
    sort(AlphabetWithDuplicates, Alphabet),
    statesFromTF(TF, StatesWithDuplicates),
    sort(StatesWithDuplicates, States),
    % Czy w zbiorze F nie ma powtórzeń?
    sort(F, AcceptingStates),
    areListsEqualLength(F, AcceptingStates),
    % Czy alfabet jest niepusty?
    head(Alphabet, _),
    % Czy stan początkowy jest w zbiorze stanów?
    member(Q0, States),         
    % Czy wszystkie stany akceptujące są w zbiorze stanów?
    allMembers(F, States),
    % Czy funkcja przejścia moze być funkcją? liczba przejść == liczba stanów razy liczba liter TODO
    isPossiblyFunction(TF, Alphabet, States),
    % Czy funkcja przejścia jest całkowita?
    isFunctionComplete(States, Alphabet, TF),
    !, % TODO usunac moze
    dfaRepresentation(TF, Q0, F, Alphabet, States, Representation).

% -----------------------------------------------------------------------------
%                                  accept 
% -----------------------------------------------------------------------------

accept([], State, dfaRepr(_, _, AcceptingStatesBST, _, _, _, _)) :-
    memberBST(State, AcceptingStatesBST).

accept([Letter|Tail], State, DFA) :-
    DFA = dfaRepr(_, _, _, TransitionsBST, _, _, _),
    accept(Tail, NextState, DFA),
    findBST(k(State, Letter), TransitionsBST, NextState).

acceptGround([], State, dfaRepr(_, _, AcceptingStatesBST, _, _, _, _)) :-
    memberBST(State, AcceptingStatesBST).

acceptGround([Letter|Tail], State, DFA) :-
    DFA = dfaRepr(_, F, AcceptingStatesBST, TransitionsBST, _, _, _),
    findBST(k(State, Letter), TransitionsBST, NextState),
    acceptGround(Tail, NextState, DFA).

% accept(+Automat, ?Słowo)
accept(A, Word) :- 
    correct(A, DFA),
    DFA = dfaRepr(Q0, _, _, _, _, _, _),
    ( ground(Word) -> acceptGround(Word, Q0, DFA) ; accept(Word, Q0, DFA) ).

% -----------------------------------------------------------------------------
%                                  empty 
% -----------------------------------------------------------------------------

acceptingStateReachable(State, AcceptingStatesBST, _, _) :-
    memberBST(State, AcceptingStatesBST).

acceptingStateReachable(State, AcceptingStatesBST, TransitionsBST, VisitedStates) :-
    \+ member(State, VisitedStates),
    findBST(k(State, _), TransitionsBST, NextState),
    acceptingStateReachable(NextState, AcceptingStatesBST, TransitionsBST, [State|VisitedStates]).

% empty(+Automat)
empty(A) :-
    correct(A, DFA),
    DFA = dfaRepr(Q0, _, AcceptingStatesBST, TransitionsBST, _, _, _),
    \+ acceptingStateReachable(Q0, AcceptingStatesBST, TransitionsBST, _).

% -----------------------------------------------------------------------------
%                                  subsetEq
% -----------------------------------------------------------------------------

% notAcceptingStates(+States, +AcceptingStatesBST, -NotAcceptingStates)
notAcceptingStates([], AcceptingStatesBST, []).

notAcceptingStates([State|StatesTail], AcceptingStatesBST, NotAcceptingBST) :-
    memberBST(State, AcceptingStatesBST),
    notAcceptingStates(StatesTail, AcceptingStatesBST, NotAcceptingBST).

notAcceptingStates([State|StatesTail], AcceptingStatesBST, [State|NotAcceptingBSTTail]) :-
    notAcceptingStates(StatesTail, AcceptingStatesBST, NotAcceptingBSTTail).

% complementDFA(+Automata, -ComplementAutomata)
complementDFA(DFA, Complement) :-
    DFA = dfaRepr(Q0, F, AcceptingStatesBST, TransitionsBST, Alphabet, _, States),
    notAcceptingStates(States, AcceptingStatesBST, NotAcceptingStates),
    initBST(NotAcceptingStates, nil, NotAcceptingStatesBST),
    Complement = dfaRepr(
        Q0, NotAcceptingStates, NotAcceptingStatesBST, 
        TransitionsBST, Alphabet, _, States
    ).

prodLists([], [], _, []).

prodLists([], [_:F2Tail], L1, F_3) :-
    prodLists(L1, F2Tail, L1, F_3).

prodLists([X|F1Tail], [Y:F2Tail], L1, [tup(X, Y)|F_3BSTTail]) :-
    prodLists(F1Tail, [Y:F2Tail], L1, F_3BSTTail).

% tfToBST(+TF, -TransitionsBST)
statesAlphabetProdAsBST([], nil).
statesAlphabetProdAsBST([tup(tup(S1_1, S1_2), C) | TFTail], TransitionsBST1, TransitionsBST2, TransitionsBST) :-
    statesAlphabetProdAsBST(TFTail, TransitionsBST1, TransitionsBST2, TransitionsBSTTail),
    findBST(k(S1_1, C), TransitionsBST1, S2_1),
    findBST(k(S1_2, C), TransitionsBST2, S2_2),
    insertBST(kv(k(tup(S1_1, S1_2), C), tup(S2_1, S2_2)), TransitionsBSTTail, TransitionsBST).


% complementDFA(+Automata, +Automata, -IntersectionAutomata)
intersectDFA(A, B, Intersection) :-
    A = dfaRepr(Q0_1, F_1, _, TransitionsBST1, Alphabet, _, States1),
    B = dfaRepr(Q0_2, F_2, _, TransitionsBST2, _, _, States2),
    Q0 = tup(Q0_1, Q0_2),
    prodLists(F_1, F_2, F_1, FProd),
    initBST(F_Itersect, nil, AcceptingStatesBST),
    prodLists(States1, States2, States1, StatesProd),
    prodLists(StatesProd, Alphabet, StatesProd, TFList),
    statesAlphabetProdAsBST(TFList, TransitionsBST1, TransitionsBST2, TransitionsBST),

    Intersection = dfaRepr(
        Q0, FProd, AcceptingStatesBST, 
        TransitionsBST, Alphabet, x, StatesProd
    ).

% subsetEq(+Automat1, +Automat2)
subsetEq(A1, A2) :-
    correct(A1, DFA1),
    correct(A2, DFA2),
    DFA1 = dfaRepr(_, _, _, _, Alphabet1, _, _),
    DFA2 = dfaRepr(_, _, _, _, Alphabet2, _, _),
    allMembers(Alphabet1, Alphabet2),
    areListsEqualLength(Alphabet2, Alphabet1),
    complementDFA(DFA2, DFA2Complement),
    intersectDFA(DFA1, DFA2Complement, Intersection),
    empty(Intersection).

% -----------------------------------------------------------------------------
%                                  equal
% -----------------------------------------------------------------------------

% equal(+Automat1, +Automat2)
equal(A1, A2) :-
    subsetEq(A1, A2),
    subsetEq(A2, A1). 
