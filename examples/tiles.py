import sys
from random import random

from facile import Goal, constraint, solve, variable

data = [
    {"sizes": [2, 1, 1, 1, 1, 1], "bigsize": 3},
    {
        "sizes": [10, 9, 7, 6, 4, 4, 3, 3, 3, 3, 3, 2, 2, 2, 1, 1, 1, 1, 1, 1],
        "bigsize": 19,
    },
    # fmt: off
    {
        "sizes": [
            50,
            42,
            37,
            35,
            33,
            29,
            27,
            25,
            24,
            19,
            18,
            17,
            16,
            15,
            11,
            9,
            8,
            7,
            6,
            4,
            2,
        ],
        "bigsize": 112,
    },
    {
        "sizes": [
            81,
            64,
            56,
            55,
            51,
            43,
            39,
            38,
            35,
            33,
            31,
            30,
            29,
            20,
            18,
            16,
            14,
            9,
            8,
            5,
            4,
            3,
            2,
            1,
        ],
        "bigsize": 175,
    },
    # fmt: on
]


def tiles(sizes, bigsize):
    n = len(sizes)
    xs = [variable(range(bigsize - sizes[i] + 1)) for i in range(n)]
    ys = [variable(range(bigsize - sizes[i] + 1)) for i in range(n)]

    for i in range(n - 1):
        for j in range(i + 1, n):
            c_left = xs[j] + sizes[j] <= xs[i]  # j on the left of i
            c_right = xs[j] >= xs[i] + sizes[i]  # j on the right of i
            c_below = ys[j] + sizes[j] <= ys[i]  # etc.
            c_above = ys[j] >= ys[i] + sizes[i]
            constraint(c_left | c_right | c_below | c_above)

    # Redundant capacity constraint
    def full_line(xy):
        for i in range(bigsize):
            # equivalent to (xy[j] >= i - sizes[j] + 1) & (xy[j] <= i)
            intersections = [
                xy[j].in_interval(i - sizes[j] + 1, i) for j in range(n)
            ]

            scal_prod = sum([s * k for s, k in zip(sizes, intersections)])
            constraint(scal_prod == bigsize)

    full_line(xs)
    full_line(ys)

    gx = Goal.forall(xs, assign="assign", strategy="min_min")
    gy = Goal.forall(ys, assign="assign", strategy="min_min")

    # Now the proper resolution process
    solution = solve(gx & gy, backtrack=True)
    print(solution)
    try:
        import matplotlib.pyplot as plt  # type: ignore

        fig, ax = plt.subplots(figsize=(7, 7))

        def fill_square(x, y, s):
            plt.fill(
                [x, x, x + s, x + s],
                [y, y + s, y + s, y],
                color=plt.get_cmap("tab20")(random()),
            )

        fill_square(0, 0, bigsize)
        for x, y, s in zip(xs, ys, sizes):
            fill_square(x.value(), y.value(), s)

        ax.set_xlim((0, bigsize))
        ax.set_ylim((0, bigsize))
        ax.set_aspect(1)
        ax.set_xticks(range(bigsize + 1))
        ax.set_yticks(range(bigsize + 1))
        ax.set_frame_on(False)
        plt.pause(10)

    except ImportError as e:
        # if matplotlib fails for an unknown reason
        print(e)
        for x, y, s in zip(xs, ys, sizes):
            print(x.value(), y.value(), s)


if __name__ == "__main__":
    try:
        tiles(**data[int(sys.argv[1])])
    except Exception as e:
        print("Usage: tiles.py {0, 1, 2, 3}")
        print(e)
