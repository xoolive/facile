import facile

# Magical sequence!
# The value inside array[i] is equal to the number of i in array

array = [facile.variable(range(10)) for i in range(10)]

for i in range(10):
    facile.constraint(sum(x == i for x in array) == array[i])  # type: ignore

if facile.solve(array):
    print([v.value() for v in array])
