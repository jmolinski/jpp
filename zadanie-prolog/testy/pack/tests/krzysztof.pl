%py expect_single_solution()
test1_correct(S) :- krzysztof(test1, A), correct(A, S).
test2_correct(S) :- krzysztof(test2, A), correct(A, S).

%py expect_success()
test_test1_accept(R) :- krzysztof(test1, A), accept(A, R).
%py assert compare_word_sets(result, [['a'], ['a', 'a'], ['b'], ['b', 'a'], ['b', 'b'], ['b', 'b', 'a']])
test_test2_accept(R) :- krzysztof(test2, A), accept(A, R).
%py assert compare_word_sets(result, [['a'], ['a', 'a'], ['b'], ['b', 'a'], ['b', 'b'], ['b', 'b', 'a']])

