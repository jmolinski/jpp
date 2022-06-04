def compare_word_sets(a, b):
    return set([tuple(x) for x in a]) == set([tuple(x) for x in b])
