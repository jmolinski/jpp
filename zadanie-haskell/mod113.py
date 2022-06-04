import random

def random_num(l):
    alph = "0123456789"
    s = ""
    for _ in range(l):
        s += random.choice(alph)
    return s

def mod113(a, b):
    n = int(str(a) + str(b))
    return (n%11) == 3


m, nm = [], []
for i in range(0, 100000):
    n = random_num(251)
    if mod113("16", n) != mod113("71", n):
        nm.append(i)
    else:
        m.append(i)


print(len(m), len(nm))
