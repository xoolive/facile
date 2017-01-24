"""
Basic examples of CSP problems:
    - a ≠ b
    - alldifferent(a, b, c) and a + b <= 2c
"""

from facile import variable, constraint, solve, alldifferent

a = variable(range(2))
b = variable(range(2))

constraint(a != b)

if solve([a, b]):
    print("Solution found a=%d and b=%d" % (a.value(), b.value()))

a = variable(range(3))
b = variable(range(3))
c = variable(range(3))

constraint(alldifferent([a, b, c]))
constraint(a + b <= 2 * c)
if solve([a, b, c]):
    print("Solution found a=%d, b=%d and c=%d" %
          (a.value(), b.value(), c.value()))
