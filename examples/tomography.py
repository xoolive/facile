"""
  This is a little 'tomography' problem, taken from an old issue
  of Scientific American.
  A matrix which contains zeroes and ones gets "x-rayed" vertically and
  horizontally, giving the total number of ones in each row and column.
  The problem is to reconstruct the contents of the matrix from this
  information.

"""

from facile import *

row_sums = [0, 0, 8, 2, 6, 4, 5, 3, 7, 0, 0]
col_sums = [0, 0, 7, 1, 6, 3, 4, 5, 2, 7, 0, 0]

rows = len(row_sums)
cols = len(col_sums)

x = [[Variable.binary() for j in col_sums] for i in row_sums]

for i in range(rows):
    constraint(sum(x[i][j] for j in range(cols)) == row_sums[i])
for j in range(cols):
    constraint(sum(x[i][j] for i in range(rows)) == col_sums[j])

def print_solution():
    print(" ", end=' ')
    for j in range(cols):
        print(col_sums[j], end=' ')
    print()
    for i in range(rows):
        print(row_sums[i], end=' ')
        for j in range(cols):
           if x[i][j].value() == 1:
               print("#", end=' ')
           else:
               print(".", end=' ')
        print()

# This problem is actually solved by simple propagation...
print_solution()

# res = solve_all([ x[i][j] for i in range(rows) for j in range(cols) ],
#         backtrack=True, on_solution=print_solution)
