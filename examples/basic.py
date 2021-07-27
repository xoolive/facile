"""
Basic examples of CSP problems:
    - a ≠ b
    - alldifferent(a, b, c) and a + b <= 2c
"""

from facile import alldifferent, constraint, solve, variable

a = variable(range(2))
b = variable(range(2))

constraint(a != b)

if solve([a, b]):
    print(f"Solution found a={a.value()} and b={b.value()}")

a = variable(range(3))
b = variable(range(3))
c = variable(range(3))

constraint(alldifferent([a, b, c]))
constraint(a + b <= 2 * c)
if solve([a, b, c]):
    print(f"Solution found a={a.value()} b={b.value()} c={c.value()}")
