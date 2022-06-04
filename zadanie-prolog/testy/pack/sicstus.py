import subprocess
import shutil


has_sicstus = shutil.which('sicstus') is not None


def upload_to_students():
    if has_sicstus:
        return
    subprocess.run(['rsync', '-r', 'prolog_utils', 'tests', 'solution.pl', 'mim:prolog'], check=True)


def execute_sicstus(filename):
    if has_sicstus:
        return subprocess.Popen(['sicstus', '-l', filename], stdout=subprocess.PIPE)
    return subprocess.Popen(['ssh', 'mim', 'cd', 'prolog', '&&', '/opt/sicstus/bin/sicstus', '-l', filename], stdout=subprocess.PIPE)
