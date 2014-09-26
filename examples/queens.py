# -*- coding: utf-8 -*-

from facile import variable, alldifferent, solve


def n_queen(n):
    """Solves the n-queen problem. """
    queens = [variable(0, n-1) for i in range(n)]
    diag1 = [queens[i] + i for i in range(n)]
    diag2 = [queens[i] - i for i in range(n)]

    alldifferent(queens)
    alldifferent(diag1)
    alldifferent(diag2)

    if solve(queens):
        return [x.value() for x in queens]
    else:
        return None


def print_line(val, n):
    cumul = ""
    for i in range(n):
        if val == i:
            cumul = cumul + "o "
        else:
            cumul = cumul + "- "
    print (cumul)


if __name__ == "__main__":
    solution = n_queen(8)
    if solution is not None:
        print ("Solution for the 8-queen problem")
        [print_line(s, 8) for s in solution]
    else:
        print ("No solution found")
