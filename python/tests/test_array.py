import numpy as np
import pytest

import facile


def test_basic_array() -> None:
    var_list = [facile.variable(0, 1) for _ in range(5)]
    array = facile.array(var_list)
    x = facile.variable(range(10))

    msg = "list indices must be integers or slices, not facile.core.Variable"
    with pytest.raises(TypeError, match=msg):
        facile.constraint(var_list[x] == 1)  # type: ignore

    facile.constraint(array[x] == 1)
    facile.constraint(array.sum() == 1)
    facile.constraint(x == 1)

    solution = facile.solve([*array, x])
    assert solution.solved
    assert array.value() == [0, 1, 0, 0, 0]


def test_2d_array() -> None:
    n = 5
    # array = facile.Array.binary((n, n))
    var_array = [[facile.variable(0, 1) for _ in range(n)] for _ in range(n)]
    array = facile.array(np.array(var_array))

    for i in range(n):
        facile.constraint(array[:, i].sum() == 1)
        facile.constraint(array[i, :].sum() == 1)

    x, y = facile.variable(range(n)), facile.variable(range(n))
    facile.constraint(array[x, y] == 1)
    # TODO redundant but necessary to test one of the arguments as a variable
    # facile.constraint(array[:, x].sum() == 1)

    sol = facile.solve([*array])
    assert sol.solved

    sol = facile.solve([*array, x, y])
    assert sol.solved
    *_, x_, y_ = sol.solution
    assert array[x_, y_].value() == 1


def test_broadcast() -> None:
    c = np.array(
        [
            [13, 21, 20, 12, 8, 26, 22, 11],
            [12, 36, 25, 41, 40, 11, 4, 8],
            [35, 32, 13, 36, 26, 21, 13, 37],
            [34, 54, 7, 8, 12, 22, 11, 40],
            [21, 6, 45, 18, 24, 34, 12, 48],
            [42, 19, 39, 15, 14, 16, 28, 46],
            [16, 34, 38, 3, 34, 40, 22, 24],
            [26, 20, 5, 17, 45, 31, 37, 43],
        ]
    )

    var = facile.Array.binary(c.shape)

    for i in range(c.shape[0]):
        facile.constraint(var[:, i].sum() == 1)
        facile.constraint(var[i, :].sum() == 1)

    # TODO (var * c).sum()
    sol = facile.minimize(list(var), (var * c.ravel()).sum())

    assert sol.solved
    assert sol.evaluation == 76
    assert (
        np.array(sol.solution).reshape(c.shape)
        == np.array(  # noqa: W503
            [
                [1, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 1],
                [0, 0, 0, 0, 0, 0, 1, 0],
                [0, 0, 0, 0, 1, 0, 0, 0],
                [0, 1, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 1, 0, 0],
                [0, 0, 0, 1, 0, 0, 0, 0],
                [0, 0, 1, 0, 0, 0, 0, 0],
            ]
        )
    ).all()
