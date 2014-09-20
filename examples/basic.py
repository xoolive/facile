# -*- coding: utf-8 -*-

"""
Basic examples of CSP problems:
    - a ≠ b
    - alldifferent(a, b, c) and a + b <= 2c
"""

from facile import variable, constraint, solve, alldifferent

a = variable(0, 1)
b = variable(0, 1)

constraint(a != b)

if solve([a, b]):
    print ("Solution found a=%d and b=%d" % (a.value(), b.value()))

a = variable(0, 2)
b = variable(0, 2)
c = variable(0, 2)

alldifferent([a, b, c])
constraint(a + b <= 2 * c)
if solve([a, b, c]):
    print ("Solution found a=%d, b=%d and c=%d" %
           (a.value(), b.value(), c.value()))
