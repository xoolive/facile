# Someone in Dreadsbury Mansion killed Aunt Agatha. Agatha, the butler, and
# Charles live in Dreadsbury Mansion, and are the only ones to live there. A
# killer always hates, and is no richer than his victim. Charles hates noone
# that Agatha hates. Agatha hates everybody except the butler. The butler hates
# everyone not richer than Aunt Agatha. The butler hates everyone whom Agatha
# hates. Noone hates everyone. Who killed Agatha?
#
# Originally from F. J. Pelletier:
#   Seventy-five problems for testing automatic theorem provers.
#   Journal of Automated Reasoning, 2: 216, 1986.

from facile import Array, constraint, solve, sum, variable

n = 3
agatha, butler, charles = 0, 1, 2

killer = variable(range(3))
victim = variable(range(3))

hates = Array.binary((n, n))
richer = Array.binary((n, n))

# Agatha, the butler, and Charles live in Dreadsbury Mansion, and
# are the only ones to live there.

# A killer always hates, and is no richer than his victim.
constraint(hates[killer, victim] == 1)
constraint(richer[killer, victim] == 0)

# No one is richer than him-/herself
for i in range(n):
    constraint(richer[i, i] == 0)

# (contd...) if i is richer than j then j is not richer than i
#  (i != j) => (richer[i, j] = 1) <=> (richer[j, i] = 0),
for i in range(n):
    for j in range(n):
        if i != j:
            c1 = richer[i, j] == 1
            c2 = richer[j, i] == 0
            constraint(c1 == c2)

# Charles hates noone that Agatha hates.
#  (hates[agatha, i] = 1) => (hates[charles, i] = 0),
for i in range(n):
    constraint((hates[agatha, i] == 1) <= (hates[charles, i] == 0))

# Agatha hates everybody except the butler.
constraint(hates[agatha, charles] == 1)
constraint(hates[agatha, agatha] == 1)
constraint(hates[agatha, butler] == 0)

# The butler hates everyone not richer than Aunt Agatha.
#   (richer[i, agatha] = 0) => (hates[butler, i] = 1),
for i in range(n):
    constraint((richer[i, agatha] == 0) <= (hates[butler, i] == 1))

# The butler hates everyone whom Agatha hates.
#   (hates[agatha, i] = 1) => (hates[butler, i] = 1),
for i in range(n):
    constraint((hates[agatha, i] == 1) <= (hates[butler, i] == 1))

# No one hates everyone.
#   (sum j: hates[i, j]) <= 2,
for i in range(n):
    constraint(sum([hates[i, j] for j in range(n)]) <= 2)

# Who killed Agatha?
constraint(victim == agatha)

assert solve(list(hates) + list(richer) + [victim, killer])
killer_value = killer.value()
assert killer_value is not None

msg = "{} killed Agatha."
print(msg.format(["Agatha", "The butler", "Charles"][killer_value]))
