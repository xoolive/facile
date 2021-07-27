import sys
from typing import Optional

from facile import Solution, alldifferent, constraint, solve, variable


def queen_strategy(queens) -> int:
    """Implements the optimal strategy for n-queen.

    The strategy selects first the queen with the smallest domain;
    then the smallest min value in the domain."""
    if len([q.value() for q in queens if q.value() is None]) == 0:
        return -1
    else:
        min_ = min(
            (len(q.domain()), q.min(), i)
            for i, q in enumerate(queens)
            if q.value() is None
        )
        return min_[-1]  # argmin is the last element in the tuple


def n_queen(n: int) -> Optional[Solution]:
    """Solves the n-queen problem."""

    queens = [variable(range(n)) for i in range(n)]

    # prepare for animation
    def on_bt(nb_bt):
        for i, q in enumerate(queens):
            print(i, q.domain())

    constraint(alldifferent(queens))
    constraint(alldifferent(queens[i] - i for i in range(n)))
    constraint(alldifferent(queens[i] + i for i in range(n)))

    return solve(queens, strategy=queen_strategy, backtrack=True)


def print_line(val: int, n: int) -> None:
    cumul = ""
    for i in range(n):
        if val == i:
            cumul = cumul + "♛ "
        else:
            cumul = cumul + "- "
    print(cumul)


if __name__ == "__main__":

    try:
        n = int(sys.argv[1])
    except Exception:
        n = 8

    solution = n_queen(n)

    if solution is not None:
        print("Solution for the {}-queen problem".format(n))
        print(solution)
        if n < 40:
            for s in solution.solution:
                print_line(s, n)
    else:
        print("No solution found")
