import os
import sys
from common import *
from pathlib import Path


def convert_to_prolog(dfa):
    ret = 'dfa('
    ret += '[' + ', '.join([f'fp({a}, {b}, {c})' for (a, b, c) in dfa.transitions]) + ']'
    ret += f', {dfa.start_state}, '
    ret += '[' + ', '.join(dfa.accept_states) + ']'
    ret += ')'
    return ret


def convert_automata_file_to_prolog(path):
    name = path.name
    if name.endswith('.txt'):
        name = name[:-4]
    with open(path) as f:
        dfas = parse_dfa_file(f)
    return [name + '(' + dfa_name + ', ' + convert_to_prolog(dfa) + ').' for dfa_name, dfa in dfas.items()]


def create_automata_file():
    result = []
    for file_path in AUTOMATA_DIR.glob('*.txt'):
        result += convert_automata_file_to_prolog(file_path)

    with open(TESTS_DIR / 'automata.pl', 'w') as f:
        f.write('% This file was automatically generated by testfile2prolog.py.\n')
        f.write('% Please do not edit it manually and instead edit the files in the automata directory.\n')
        f.write('\n')
        for l in result:
            f.write(l)
            f.write('\n')


if __name__ == '__main__':
    create_automata_file()