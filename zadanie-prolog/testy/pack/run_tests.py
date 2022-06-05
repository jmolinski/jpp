import traceback

from common import *
from testfile2prolog import create_automata_file
from sicstus import upload_to_students, execute_sicstus
import utils


def parse_test_file(file_path):
    blocks = []
    py_mode = False

    with open(file_path) as f:
        line_no = -1
        for line in f:
            line_no += 1
            if py_mode:
                if line.strip() == '*/':
                    py_mode = False
                    continue

                blocks.append({'t': 'python', 'l': line_no, 'code': line.rstrip()})
                continue

            p = PrologParser(line.rstrip())
            pred = p.parse_simple_predicate()
            if pred is not None and pred[0].startswith('test'):  # ensure only test* predicates get ran
                blocks.append({'t': 'pred', 'l': line_no, 'pred': pred})
                continue

            if line.startswith('%py '):
                blocks.append({'t': 'python', 'l': line_no, 'code': '    ' + line[4:], 'inline': True})
                continue
            if line.startswith('%timeout '):
                blocks.append({'t': 'timeout', 'value': int(line[len('%timeout '):])})
                continue

            if line.startswith('/*py'):
                py_mode = True
                continue

    return blocks


def generate_prolog_file(test_file_name, blocks):
    with open('prolog_utils/test_tmp.pl', 'w') as f:
        f.write(':- consult(\'test_lib\').\n')
        f.write(':- consult(\'../tests/automata\').\n')
        f.write(':- consult(\'../solution\').\n')
        f.write(':- consult(\'../tests/' + test_file_name + '\').\n')
        result_count = 100
        timeout = 100
        for blk in blocks:
            if blk['t'] == 'timeout':
                timeout = blk['value']
            if blk['t'] == 'pred':
                (pred_name, pred_vcount) = blk['pred']
                vars = [f'V{i}' for i in range(pred_vcount)]
                var_names = []
                call_pred = pred_name
                if pred_vcount > 0:
                    call_pred = pred_name + '(' + ','.join(vars) + ')'
                    var_names = [f'\'result{i+1}\'' for i in range(pred_vcount)] if pred_vcount > 1 else ['\'result\'']
                f.write(f':- run_test({pred_name}, {call_pred}, [' + ','.join(vars) + '], [' + ','.join(var_names) + f'], {result_count}, {timeout}).\n')
        f.write(':- halt.\n')


class TestRunner:
    def __init__(self, proc):
        self.proc = proc
        self.globals = None
        self.solutions = {}
        self.solution_counts = {}
        self.prolog_finished = {}
        self.finished = set()
        self.should_expect = None

    def finish_pred(self, name):
        if name is None or name in self.finished:
            return
        print('[PASS] ' + name)
        self.finished.add(name)

    def fail_pred(self, name, error):
        if name is None:
            return
        print('[FAIL] ' + name + ': ' + error)
        self.finished.add(name)

    def wait_for_pred(self, name):
        if name in self.prolog_finished:
            self.assign_results(name)
            return

        while True:
            line = self.proc.stdout.readline()
            if not line:
                break
            line = line.decode().strip()
            cmd, _, args = line.partition(':')
            if cmd == 'TestStart':
                test_name = args
                self.solutions[test_name] = {}
                self.solution_counts[test_name] = 0
                # print('[....] ' + test_name)
            elif cmd == 'TestEnd':
                [test_name, result] = args.split(':', 2)
                self.prolog_finished[test_name] = result
                if result != 'success':
                    self.fail_pred(name, result)
                if test_name == name:
                    break
            elif cmd == 'TestSolution':
                try:
                    [test_name, var_name, var_value] = args.split(':', 3)
                except:  # can be corrupted on a timeout
                    continue
                if var_name == 'nil':
                    self.solution_counts[test_name] += 1
                    continue
                if var_name not in self.solutions[test_name]:
                    self.solutions[test_name][var_name] = []
                try:
                    var_value = PrologParser(var_value).parse_value()
                except:
                    continue
                self.solutions[test_name][var_name].append(var_value)
        self.assign_results(name)

    def assign_results(self, name):
        self.do_result_check(name)
        self.globals['result'] = []
        for k, v in self.solutions[name].items():
            self.globals[k] = v

    def do_result_check(self, name):
        if self.should_expect == 'success':
            if self.solution_counts[name] <= 0:
                self.fail_pred(name, 'did not succeed')
        if self.should_expect == 'single_solution':
            if self.solution_counts[name] != 1:
                self.fail_pred(name, 'did not succeed with a single solution')
        if self.should_expect == 'fail':
            if self.solution_counts[name] > 0:
                self.fail_pred(name, 'did not fail')

    def expect_success(self):
        self.should_expect = 'success'

    def expect_single_solution(self):
        self.should_expect = 'single_solution'

    def expect_fail(self):
        self.should_expect = 'fail'


def run_tests_in_file(file_path):
    print('Running tests in: ' + str(file_path))
    test_file_name = file_path.name
    if test_file_name.endswith('.pl'):
        test_file_name = test_file_name[:-3]

    blocks = parse_test_file(file_path)
    generate_prolog_file(test_file_name, blocks)

    upload_to_students()
    proc = execute_sicstus('prolog_utils/test_tmp.pl')

    runner = TestRunner(proc)
    runner.globals = {
        'expect_success': runner.expect_success,
        'expect_single_solution': runner.expect_single_solution,
        'expect_fail': runner.expect_fail,
        'compare_word_sets': utils.compare_word_sets
    }

    current_pred = None
    for blk in blocks:
        if blk['t'] == 'pred':
            (pred_name, _) = blk['pred']
            runner.finish_pred(current_pred)
            current_pred = pred_name
            runner.wait_for_pred(pred_name)
        if blk['t'] == 'python':
            try:
                if 'inline' in blk:
                    code_txt = '\n' * (blk['l'] - 1) + 'if True:\n' + blk['code']
                else:
                    code_txt = '\n' * blk['l'] + blk['code']
                code = compile(code_txt, file_path.absolute(), 'exec')
                exec(code, runner.globals, runner.globals)
            except Exception as e:
                if current_pred is not None:
                    runner.fail_pred(current_pred, 'Python error')
                    traceback.print_exc()
                    runner.finish_pred(current_pred)
    runner.finish_pred(current_pred)


create_automata_file()
for file_path in TESTS_DIR.glob('*.pl'):
    if file_path != TESTS_DIR / 'automata.pl':
        run_tests_in_file(file_path)
