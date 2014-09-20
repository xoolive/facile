# -*- coding: utf-8 -*-

"""
Find four numbers such that their sum is 711 and their product is 711000000
"""

from facile import variable, constraint, solve

a = variable(0, 330)
b = variable(0, 160)
c = variable(0, 140)
d = variable(0, 140)

constraint(a + b + c + d == 711)
constraint(a * b * c * d == 711000000)

if solve([a, b, c, d]):
    [va, vb, vc, vd] = [x.value() for x in [a, b, c, d]]
    print ("Solution found a=%d, b=%d, c=%d, d=%d" % (va, vb, vc, vd))
else:
    print ("No solution found")
