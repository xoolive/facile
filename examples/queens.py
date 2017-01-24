from facile import variable, constraint, alldifferent, solve


def n_queen(n):
    """Solves the n-queen problem. """

    queens = [variable(range(n)) for i in range(n)]

    # prepare for animation
    def on_bt(nb_bt):
        for i, q in enumerate(queens):
            print(i, q.domain())

    constraint(alldifferent(queens))
    constraint(alldifferent(queens[i] - i for i in range(n)))
    constraint(alldifferent(queens[i] + i for i in range(n)))

    return solve(queens, strategy="queen", backtrack=True)


def print_line(val, n):
    cumul = ""
    for i in range(n):
        if val == i:
            cumul = cumul + "♛ "
        else:
            cumul = cumul + "- "
    print(cumul)


if __name__ == "__main__":
    import sys
    try:
        n = int(sys.argv[1])
    except:
        n = 8
    solution = n_queen(n)
    if solution is not None:
        print("Solution for the {}-queen problem".format(n))
        print(solution)
        if n < 40:
            [print_line(s, n) for s in solution.solution]
    else:
        print("No solution found")
