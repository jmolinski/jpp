from common import *
import subprocess


GRAPHVIZ_DIR = Path('graphviz')
GRAPHVIZ_DIR.mkdir(exist_ok=True)
(GRAPHVIZ_DIR / 'src').mkdir(exist_ok=True)

for file_path in AUTOMATA_DIR.glob('*.txt'):
    name = file_path.name
    if name.endswith('.txt'):
        name = name[:-4]

    with open(file_path) as f:
        dfas = parse_dfa_file(f)
    for dfa_name, dfa in dfas.items():
        if '/' in dfa_name:
            raise Exception('invalid dfa name!')
        out_file = GRAPHVIZ_DIR / 'src' / (name + '_' + dfa_name + '.dot')
        out_png_file = GRAPHVIZ_DIR / (name + '_' + dfa_name + '.png')
        with open(out_file, 'w') as f:
            f.write('digraph finite_state_machine {\n')
            f.write('	fontname="Helvetica,Arial,sans-serif"\n')
            f.write('	node [fontname="Helvetica,Arial,sans-serif"]\n')
            f.write('	edge [fontname="Helvetica,Arial,sans-serif"]\n')
            f.write('	rankdir=LR;\n')
            if dfa.start_state not in dfa.accept_states:
                f.write('	node [shape = circle]; ' + dfa.start_state + ';\n')
            else:
                f.write('	node [shape = doublecircle]; ' + dfa.start_state + ';\n')
            accept_states = [x for x in dfa.accept_states if x != dfa.start_state]
            if len(accept_states) > 0:
                f.write('	node [shape = doublecircle]; ' + ' '.join(accept_states) + ';\n')
            f.write('	node [shape = circle];\n')
            for (t_from, t_label, t_to) in dfa.transitions:
                f.write(f'	{t_from} -> {t_to} [label = "{t_label}"];\n')

            f.write('	nowhere [shape=point];')
            f.write('	nowhere -> ' + dfa.start_state + ';')
            f.write('}')
        subprocess.run(['dot', '-o', str(out_png_file), '-Tpng:gd', str(out_file)], check=True)
