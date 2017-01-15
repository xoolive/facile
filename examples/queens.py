from facile import array, variable, constraint, alldifferent, solve, Strategy


def n_queen(n):
    """Solves the n-queen problem. """

    queens = array(variable(0, n - 1) for i in range(n))

    constraint(alldifferent(queens))
    constraint(alldifferent(queens[i] - i for i in range(n)))
    constraint(alldifferent(queens[i] + i for i in range(n)))

    if solve(queens, strategy=Strategy.min_min()):
        return [x.value() for x in queens]
    else:
        return None


def print_line(val, n):
    cumul = ""
    for i in range(n):
        if val == i:
            cumul = cumul + "♛ "
        else:
            cumul = cumul + "- "
    print(cumul)


if __name__ == "__main__":
    solution = n_queen(8)
    if solution is not None:
        print("Solution for the 8-queen problem")
        [print_line(s, 8) for s in solution]
    else:
        print("No solution found")
