import facile

# A Golf Tournament (from http://www.icparc.ic.ac.uk/~cg6/conjunto.html)
#
# There are 32 golfers, who play individually but in groups of 4, called
# foursomes. The tournament is organized in weeks. Each week a new set of
# foursomes has to be computed such that each person only golfs with the same
# person once. So if two golfers have played each other in any previous week
# they should not play each other in the coming weeks.
#
# The question is: "How many weeks can we ensure this before players start to
# play each other a second time ?"
#
# The formulation is generalized to any number of golfers, groups and weeks.

nb_groups = 5
nb_golfers = 15
size_group = 3

assert size_group * nb_groups == nb_golfers

nb_weeks = 5

# An array of nb_weeks * nb_golfers decision variables to choose the group of
# every golfer every week

groups = [
    facile.array([facile.variable(range(nb_groups)) for i in range(nb_golfers)])
    for j in range(nb_weeks)
]

# For each week, exactly size_group golfers in each group:
for i in range(nb_weeks):

    # [1] Use a Sorting Constraint (redundant with [2])
    s = groups[i].sort()
    for j in range(nb_golfers):
        facile.constraint(s[j] == j // size_group)

    # [2] Use a Global Cardinality Constraint (redundant with [1])
    gcc = groups[i].gcc([(size_group, i) for i in range(nb_groups)])
    facile.constraint(gcc)

# Two golfers do not play in the same group more than once
for g1 in range(nb_golfers):
    for g2 in range(g1 + 1, nb_golfers):
        g1_with_g2 = [groups[w][g1] == groups[w][g2] for w in range(nb_weeks)]
        facile.constraint(sum(g1_with_g2) <= 1)

# Breaking the symmetries
#  - 0 always in the first group, 1 in a group less than 1, ...
#  - First week (0) a priori chosen

for w in range(nb_weeks):
    for g in range(nb_groups):
        facile.constraint(groups[w][g] <= g)

for g in range(nb_golfers):
    facile.constraint(groups[0][g] == g // size_group)

if not facile.solve([v for k in groups for v in k]):
    print("No solution found")
else:
    for v in groups:
        print(v.value())
