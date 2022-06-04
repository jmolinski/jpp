from dataclasses import dataclass
from pathlib import Path
import re


AUTOMATA_DIR = Path('automata')
TESTS_DIR = Path('tests')


@dataclass
class Dfa:
    transitions: list[(str, str, str)]
    start_state: str
    accept_states: list[str]


def dfa_to_txt(dfa):
    res = ''
    res += 'start ' + dfa.start_state + '\n'
    res += 'accept ' + ' '.join(dfa.accept_states) + '\n'
    for (t_from, t_label, to_to) in dfa.transitions:
        res += f'{t_from} {t_label} {to_to}\n'
    return res


def parse_dfa_file(lines):
    dfa = None
    dfas = {}

    for l in lines:
        if l.startswith('#') or l.startswith('//') or l.startswith('%'):
            continue
        if l.strip().endswith(':'):
            label = l.strip()[:-1]
            dfa = Dfa([], '', [])
            dfas[label] = dfa
            continue
        if l.startswith('start '):
            dfa.start_state = l[len('start '):].strip()
            continue
        if l.startswith('accept '):
            dfa.accept_states += [x.strip() for x in l[len('accept '):].split(' ')]
            continue
        items = [x.strip() for x in l.split(' ')]
        if len(items) == 3:
            dfa.transitions.append((items[0], items[1], items[2]))

    return dfas


tokreg = re.compile('([a-zA-Z0-9_]+|:-|[<>=;,+\-*()\\[\\]]|[ \n]+)')


class PrologParser:
    def __init__(self, str):
        self.str = str
        self.pos = 0
        self.returned_tokens = []

    def next_token(self):
        if len(self.returned_tokens) > 0:
            ret = self.returned_tokens[0]
            del self.returned_tokens[0]
            return ret
        while True:
            res = tokreg.match(self.str, self.pos)
            if res is None:
                return None
            tok = res.group(0)
            self.pos = res.end()
            if tok.strip() == '':
                continue
            return tok

    def put_token(self, tok):
        if tok is not None:
            self.returned_tokens.append(tok)

    def maybe_eat_token(self, tok):
        nt = self.next_token()
        if nt != tok:
            self.put_token(nt)
            return False
        return True

    def eat_simple_list(self):
        while not self.maybe_eat_token(']'):
            t = self.next_token()
            if t == '[':
                self.eat_simple_list()

    def parse_simple_predicate(self):
        name = self.next_token()
        t = self.next_token()
        if t == ':-':
            return name, 0
        if t != '(':
            return None
        cnt = 1
        while not self.maybe_eat_token(')'):
            t = self.next_token()
            if t == ',':
                cnt += 1
            if t == '[':
                self.eat_simple_list()
        if self.next_token() != ':-':
            return None
        return name, cnt

    def parse_value(self):
        t = self.next_token()
        if t == '[':
            ret = []
            while not self.maybe_eat_token(']'):
                ret.append(self.parse_value())
                self.maybe_eat_token(',')
            return ret
        return t

