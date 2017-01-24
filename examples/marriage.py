from facile import array, variable, constraint, solve

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

wife = array([variable(range(n)) for i in range(n)])
husband = array([variable(range(n)) for i in range(n)])

# You are your wife's husband, and conversely
for m in range(n):
    constraint(husband[wife[m]] == m)
for w in range(n):
    constraint(wife[husband[w]] == w)

for m in range(n):
    for w in range(n):
        # m prefers this woman to his wife
        c1 = rank_men[m][w] < array(rank_men[m])[wife[m]]
        # w prefers her husband to this man
        c2 = array(rank_women[w])[husband[w]] < rank_women[w][m]
        # alias for c1 => c2
        constraint(c1 <= c2)
        # w prefers this man to her husband
        c3 = rank_women[w][m] < array(rank_women[w])[husband[w]]
        # m prefers his wife to this woman
        c4 = array(rank_men[m])[wife[m]] < rank_men[m][w]
        constraint(c3 <= c4)

if solve(list(wife) + list(husband)):
    for i in range(n):
        print("%s <=> %s" % (men[i], women[wife[i].value()]))
