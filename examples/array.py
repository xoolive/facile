import facile

# Magical sequence!

array = [facile.variable(0,10) for i in range(10)]
a = facile.array(array)

for i in range(10):
    facile.constraint(a.count_eq(i) == array[i])
# useless constraint, but for the sake of testing
facile.constraint(a[0] != 0)

if facile.solve(array):
    print ([v.value() for v in array])


# Integer definition

idx = facile.array([i for i in range(1, 14)])
a = [facile.variable(0,10) for i in range(10)]

# initialise
facile.constraint(a[0] == 1)
# recursion
for i in range(9):
    facile.constraint(idx[a[i]] == a[i+1])

if facile.solve(a):
    print ([v.value() for v in a])
