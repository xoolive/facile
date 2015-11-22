import facile

data = { 'sizes': [2, 1, 1, 1, 1, 1], 'bigsize': 3}
data = { 'sizes': [10, 9, 7, 6, 4, 4, 3, 3, 3, 3, 3, 2, 2, 2, 1, 1, 1, 1, 1, 1],
         'bigsize': 19 }
# More examples to come
# data = { 'sizes': [50, 42, 37, 35, 33, 29, 27, 25, 24, 19, 18, 17, 16, 15, 11,
#                    9, 8, 7, 6, 4, 2],
#          'bigsize': 112 },
# data = { 'sizes': [81, 64, 56, 55, 51, 43, 39, 38, 35, 33, 31, 30, 29, 20, 18,
#                    16, 14, 9, 8, 5, 4, 3, 2, 1],
#          'bigsize': 175 }

def tile(sizes, bigsize):
    n = len(sizes)
    xs = [facile.variable(0, bigsize - sizes[i]) for i in range(n)]
    ys = [facile.variable(0, bigsize - sizes[i]) for i in range(n)]

    for i in range(n-1):
        for j in range(i+1, n):
            c_left = xs[j] + sizes[j] <= xs[i] # j on the left of i
            c_right = xs[j] >= xs[i] + sizes[i] # j on the right of i
            c_below = ys[j] + sizes[j] <= ys[i] # etc.
            c_above = ys[j] >= ys[i] + sizes[i]
            facile.constraint(c_left | c_right | c_below | c_above)

    # Redundant capacity constraint
    def full_line(xy):
        for i in range(bigsize):
            # equivalent to (xy[j] >= i - sizes[j] + 1) & (xy[j] <= i)
            intersections = \
                    [xy[j].in_interval(i - sizes[j] + 1, i) for j in range(n)]

            scal_prod = sum([s * k for s, k in zip(sizes, intersections)])
            facile.constraint(scal_prod == bigsize)

    full_line(xs)
    full_line(ys)

    if facile.solve(xs + ys, heuristic=facile.Heuristic.Min_min):
        try:
            import matplotlib.pyplot as plt
            import matplotlib.cm as colormap
            from random import random as rand

            fig = plt.figure()
            ax = fig.gca()

            def fill_square(x, y, s):
                plt.fill([x, x, x+s, x+s], [y, y+s, y+s, y],
                         color=colormap.Pastel1(rand()))

            fill_square(0, 0, bigsize)
            for (x, y, s) in zip(xs, ys, sizes):
                fill_square(x.value(), y.value(), s)

            ax.set_xlim((0, bigsize))
            ax.set_ylim((0, bigsize))
            ax.set_aspect(1)
            ax.set_xticks(range(bigsize + 1))
            ax.set_yticks(range(bigsize + 1))
            fig.set_size_inches(7, 7)
            ax.set_frame_on(False)
            plt.pause(10)

        except Exception as e:
            # if matplotlib fails for an unknown reason
            print (e)
            for (x, y, s) in zip(xs, ys, sizes):
                print (x.value(), y.value(), s)


if __name__ == '__main__':
    tile(**data)
