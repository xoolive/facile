# flake8: noqa: E226

from facile import constraint, solve, sum, variable

# Number of buckets
nb = 3
# Number of steps (let's say we know... :p)
steps = 8
# The capacity of each bucket
capacity = [8, 5, 3]

buckets = [
    [variable(range(capacity[b] + 1)) for b in range(nb)] for i in range(steps)
]

constraint(buckets[0][0] == 8)
constraint(buckets[0][1] == 0)
constraint(buckets[0][2] == 0)

constraint(buckets[steps - 1][0] == 4)
constraint(buckets[steps - 1][1] == 4)
constraint(buckets[steps - 1][2] == 0)

for i in range(steps - 1):
    # we change the contents of two buckets at a time
    sum_buckets = sum([buckets[i][b] != buckets[i + 1][b] for b in range(nb)])
    constraint(sum_buckets == 2)
    # we play with a constant amount of water
    sum_water = sum([buckets[i][b] for b in range(nb)])
    constraint(sum_water == 8)
    for b1 in range(nb):
        for b2 in range(b1):
            constraint(
                # either the content of the bucket does not change
                (buckets[i][b1] == buckets[i + 1][b1])
                | (buckets[i][b2] == buckets[i + 1][b2])
                |
                # or the bucket ends up empty or full
                (buckets[i + 1][b1] == 0)
                | (buckets[i + 1][b1] == capacity[b1])
                | (buckets[i + 1][b2] == 0)
                | (buckets[i + 1][b2] == capacity[b2])
            )

if solve([b for sub in buckets for b in sub]):
    for sub in buckets:
        print([b.value() for b in sub])
