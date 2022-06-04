import sys
from common import *


def parse_prolog_dfa(p):
    assert p.next_token() == 'dfa'
    assert p.next_token() == '('

    transitions = []
    assert p.next_token() == '['
    while not p.maybe_eat_token(']'):
        assert p.next_token() == 'fp'
        assert p.next_token() == '('
        t_from = p.next_token()
        assert p.next_token() == ','
        t_label = p.next_token()
        assert p.next_token() == ','
        t_to = p.next_token()
        assert p.next_token() == ')'
        p.maybe_eat_token(',')
        transitions.append((t_from, t_label, t_to))
    assert p.next_token() == ','

    start_state = p.next_token()
    assert p.next_token() == ','

    accept_states = []
    assert p.next_token() == '['
    while not p.maybe_eat_token(']'):
        state = p.next_token()
        p.maybe_eat_token(',')
        accept_states.append(state)

    return Dfa(transitions, start_state, accept_states)


if __name__ == '__main__':
    # parses lines in the format: example(label, dfa(...))
    for l in sys.stdin:
        try:
            p = PrologParser(l)
            t = p.next_token()
            assert p.next_token() == '('
            example_label = p.next_token()
            assert p.next_token() == ','
            dfa = parse_prolog_dfa(p)
            print(example_label + ':')
            print(dfa_to_txt(dfa))
        except:
            pass
