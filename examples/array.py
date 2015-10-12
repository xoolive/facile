# Playing with some recursion with arrays indexed by variables
# Better example welcome

import facile

idx = facile.array([i for i in range(1, 14)])

a = [facile.variable(0, 10) for i in range(10)]

# initialise
facile.constraint(a[0] == 1)

# recursion
for i in range(9):
    facile.constraint(idx[a[i]] == a[i+1])

if facile.solve(a):
    print ([v.value() for v in a])
