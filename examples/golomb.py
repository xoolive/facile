# coding: utf-8

# A Golomb ruler is a set of integers (marks) a(1) < ... < a(k) such
# that all the differences a(i) - a(j) (i > j) are distinct. Clearly we
# may assume a(1) = 0. Then a(k) is the length of the Golomb ruler. For a
# given number of marks, k, we are interested in finding the shortest
# Golomb rulers. Such rulers are called optimal.

import facile

def golomb(n):
    n2 = 2 ** n
    ticks = [facile.variable(0, n2) for i in range(n)]

    # First tick at the start of the ruler
    facile.constraint(ticks[0] == 0)

    # Ticks are ordered
    for i in range(n-1):
        facile.constraint(ticks[i] < ticks[i+1])

    # All distances
    distances = []
    for i in range(n-1):
        for j in range(i + 1, n):
            distances.append(ticks[j] - ticks[i])
    facile.alldifferent(distances)

    for d in distances:
        facile.constraint(d > 0)

    # Breaking the symmetry
    size = len(distances)
    facile.constraint(distances[size - 1] > distances[0])

    return (facile.minimize(ticks, ticks[n-1])[1])

if __name__ == "__main__":
    import sys
    n = 5
    if len(sys.argv) > 1:
        n = int(sys.argv[1])
        if n > 10:
            print ("With due respect, 10 may be time-consuming enough...")
    print (golomb(n))
