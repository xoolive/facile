# coding: utf-8

import facile

n = 5
men = ["Richard", "James", "John", "Hugh", "Greg"]
women = ["Helen", "Tracy", "Linda", "Sally", "Wanda"]

rank_women = [[1, 2, 4, 3, 5],
              [3, 5, 1, 2, 4],
              [5, 4, 2, 1, 3],
              [1, 3, 5, 4, 2],
              [4, 2, 3, 5, 1]]

rank_men = [[5, 1, 2, 4, 3],
            [4, 1, 3, 2, 5],
            [5, 3, 2, 4, 1],
            [1, 5, 4, 3, 2],
            [4, 3, 2, 1, 5]]

wife = [facile.variable(0, n-1) for i in range(n)]
husband = [facile.variable(0, n-1) for i in range(n)]

for m in range(n):
    facile.constraint(facile.array(husband)[wife[m]] == m)
for w in range(n):
    facile.constraint(facile.array(wife)[husband[w]] == w)

for m in range(n):
    for w in range(n):
        # m prefers this woman to his wife
        c1 = rank_men[m][w] < facile.array(rank_men[m])[wife[m]]
        # w prefers her husband to this man
        c2 = facile.array(rank_women[w])[husband[w]] < rank_women[w][m]
        # alias for c1 => c2
        facile.constraint( c1 <= c2 )
        # w prefers this man to her husband
        c3 = rank_women[w][m] < facile.array(rank_women[w])[husband[w]]
        # m prefers his wife to this woman
        c4 = facile.array(rank_men[m])[wife[m]] < rank_men[m][w]
        facile.constraint( c3 <= c4 )

if facile.solve(wife + husband):
    for i in range(n):
        print ("%s <=> %s" % (men[i], women[wife[i].value()]))

