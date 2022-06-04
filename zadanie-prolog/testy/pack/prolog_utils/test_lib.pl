:- use_module(call_nth).
:- use_module(library(timeout)).

test_start(TestName) :-
    write('TestStart:'),
    write(TestName),
    nl.
test_solution(TestName, VarName, Solution) :-
    write('TestSolution:'),
    write(TestName),
    write(':'),
    write(VarName),
    write(':'),
    print(Solution),
    nl.
test_end(TestName, Result) :-
    write('TestEnd:'),
    write(TestName),
    write(':'),
    write(Result),
    nl.



print_solutions(TestName, [], []).
print_solutions(TestName, [Var | Vars], [VarName | VarNames]) :-
    test_solution(TestName, VarName, Var),
    print_solutions(TestName, Vars, VarNames).

print_test_solutions(TestName, Test, Vars, VarNames) :-
    Test,
    test_solution(TestName, nil, nil),
    print_solutions(TestName, Vars, VarNames).



run_test(TestName, Test, Vars, VarNames, SolutionCount, Timeout) :-
    test_start(TestName),
    time_out((call_nth(print_test_solutions(TestName, Test, Vars, VarNames), SolutionCount); true), Timeout, Result),
    nl,
    test_end(TestName, Result).

