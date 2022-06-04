~~~
Testing tool for task 3

How to run the tests:
- In order to run the tests, simply put your solution as as solution.py in this directory and run python3 run_tests.py
  on students.
- If you get failures, you can check out the tests under the tests/ directory. All the automata are defined under
  the automata/ directory and rendered as graphs in the graphviz directory. You can rerender the graphics using
  python3 graphviz.py on Linux.
- Note that you can NOT edit tests/automata.pl too easily since it's regenerated every time you use run_tests.py. You
  can comment out the create_automata_file() invocation if you are not fond of this behavior.

How to make your own tests:
- Create a new text file in automata/ for yourself, eg. automata/miko.txt.
  * Define any required automata in this file, see the syntax section below. Also check out example.txt.
- Create a new file under tests/ for yourself, eg. tests/miko.pl.
  * Define the tests in this file, see the tests section below.
- Share the newly created files with others.
- That's all. The tests will be automatically executed by run_tests.py if present.


がんばろう！


~~~
File syntax for files under automata/

Define an automaton with the label "label":
label:

Define the starting state as 1:
start 1

Define the accepting states as 2 and 1:
accept 2 1

Define a transition from 1 to 2 with the label a:
1 a 2

Please see automata/example.txt for examples. You can also use python3 prolog2testfile.py to parse existing prolog code
with automata, eg. (input is marked with > and output is marked with <):

$ python3 prolog2testfile.py
> example(a11, dfa([fp(1, a, 1), fp(1, b, 2), fp(2, a, 2), fp(2, b, 1)], 1, [2, 1])).
< a11:
< start 1
< accept 2 1
< 1 a 1
< 1 b 2
< 2 a 2
< 2 b 1


~~~
Useful info for making tests

Any predicates with names starting from test in these files are considered individual tests. These will be automatically
launched and tested.
Any lines starting with %py or /*py (single or multi line comments, immediately followed by py, without a space) are
evaluated in Python. Multiline comments must end with a */ on a separate line.
This works with a lot of magic, however basically the way it works, is that the Python script parses the test .pl file
and when it encounters one of these comments it evaluates it. When it encounters something that looks like a test
predicate it notes it down, and also waits for the predicate to be evaluated by the Prolog interpreter. Afterwards the
execution results are saved, parsed and available in the Python blocks under the result variable. Please note that any
errors in the Python execution will make the last interpreted test fail.
Only the first 100 results are available in Python and the interpreter is set to time out after 100ms by default.

In addition the following snippets can be used:

- Require all the following predicated to have at least a single solution:
%py expect_success()

- Require all the following predicated to not have any solutions:
%py expect_fail()

- Adjust the timeout to 1000ms (from the default 100ms, applies to all following predicates in the file):
%timeout 1000
