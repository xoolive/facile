# A Golomb ruler is a set of integers (marks) a(1) < ... < a(k) such
# that all the differences a(i) - a(j) (i > j) are distinct. Clearly we
# may assume a(1) = 0. Then a(k) is the length of the Golomb ruler. For a
# given number of marks, k, we are interested in finding the shortest
# Golomb rulers. Such rulers are called optimal.

import sys
from typing import Optional

import facile


def golomb(n) -> Optional[facile.Solution]:

    # On peut majorer la taille de la règle par 2 ** n. En effet, si
    # ticks[i] vaut 2**i alors tous les ticks[i] - ticks[j] = 2**i - 2**j
    # = 2**j * (2**(i-j) - 1) qui sont tous différents.
    # On a donc au moins cette solution.

    ticks = [facile.variable(range(2 ** n)) for i in range(n)]

    # First tick at the start of the ruler
    facile.constraint(ticks[0] == 0)

    # Ticks are ordered
    for i in range(n - 1):
        facile.constraint(ticks[i] < ticks[i + 1])

    # All distances
    distances = []
    for i in range(n - 1):
        for j in range(i + 1, n):
            distances.append(facile.variable(ticks[j] - ticks[i]))
    facile.constraint(facile.alldifferent(distances))

    for d in distances:
        facile.constraint(d > 0)

    # Breaking the symmetry
    facile.constraint(distances[-1] > distances[0])

    return facile.minimize(
        ticks, ticks[n - 1], backtrack=True, on_solution=print
    )


if __name__ == "__main__":

    n = 5
    if len(sys.argv) > 1:
        n = int(sys.argv[1])
        if n > 11:
            print("With due respect, n=11 may be time-consuming enough...")
    print(golomb(n))
