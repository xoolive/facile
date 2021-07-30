import numpy as np

from facile import Array, constraint, maximize

capacity = np.array([18209, 7692, 1333, 924, 26638, 61188, 13360])
value = np.array([96, 76, 56, 11, 86, 10, 66, 86, 83, 12, 9, 81])

use = np.array(
    [
        [19, 1, 10, 1, 1, 14, 152, 11, 1, 1, 1, 1],
        [0, 4, 53, 0, 0, 80, 0, 4, 5, 0, 0, 0],
        [4, 660, 3, 0, 30, 0, 3, 0, 4, 90, 0, 0],
        [7, 0, 18, 6, 770, 330, 7, 0, 0, 6, 0, 0],
        [0, 20, 0, 4, 52, 3, 0, 0, 0, 5, 4, 0],
        [0, 0, 40, 70, 4, 63, 0, 0, 60, 0, 4, 0],
        [0, 32, 0, 0, 0, 5, 0, 3, 0, 660, 0, 9],
    ]
)


def main() -> None:
    take = Array.variable(value.shape, 0, max(capacity))
    for r, c in enumerate(capacity):
        constraint(sum(use[r, i] * t for i, t in enumerate(take)) <= c)

    sol = maximize(take, (take * value).sum())

    print(sol)
    generator = ((i, t) for i, t in enumerate(sol.solution) if t > 0)
    for i, v in generator:
        msg = "Take {:>4} of item[{}]"
        print(msg.format(int(v), i))


if __name__ == "__main__":
    main()
