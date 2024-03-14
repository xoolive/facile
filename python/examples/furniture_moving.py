# https://github.com/google/or-tools/.../examples/python/furniture_moving.py
#  Moving furnitures (scheduling) problem in Google CP Solver.
#  Marriott & Stukey: 'Programming with constraints', page  112f
#  The model implements an experimental decomposition of the
#  global constraint cumulative.

import facile

n = 4
duration = [30, 10, 15, 15]
demand = [3, 1, 3, 2]
upper_limit = 160

start_times = [facile.variable(range(upper_limit)) for i in range(n)]
end_times = [facile.variable(range(2 * upper_limit)) for i in range(n)]

end_time = facile.variable(range(2 * upper_limit))
n_resources = facile.variable(range(11))

for i in range(n):
    facile.constraint(end_times[i] == start_times[i] + duration[i])

facile.constraint(end_time == facile.array(end_times).max())


# detail here!
def cumulative(s, d, r, b):
    tasks = [i for i in range(len(s)) if r[i] > 0 and d[i] > 0]
    times_min = min([s[i].domain()[0] for i in tasks])
    times_max = max([s[i].domain()[-1] + max(d) for i in tasks])
    for t in range(times_min, times_max + 1):
        bb = []
        for i in tasks:
            c1 = s[i] <= t
            c2 = t < s[i] + d[i]
            bb.append(c1 * c2 * r[i])
        facile.constraint(sum(bb) <= b)


cumulative(start_times, duration, demand, n_resources)

print("---- Minimize resources ----")
print(
    facile.minimize(
        start_times + end_times + [n_resources], n_resources, on_solution=print
    )
)

print("---- Minimize end_time ----")
print(
    facile.minimize(
        start_times + end_times + [end_time], end_time, on_solution=print
    )
)
