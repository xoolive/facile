# -*- coding: utf-8 -*-

"""
Find four numbers such that their sum is 711 and their product is 711000000
"""

from facile import constraint, solve, variable

a = variable(range(0, 330))
b = variable(range(0, 160))
c = variable(range(0, 140))
d = variable(range(0, 140))

constraint(a + b + c + d == 711)
constraint(a * b * c * d == 711000000)

sol = solve([a, b, c, d])
assert sol is not None
print("Solution found a={}, b={}, c={}, d={}".format(*sol.solution))
