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


def test_invalid_array() -> None:

    n = 5
    var_array = [[facile.variable(0, 1) for _ in range(n)] for _ in range(n)]
    np_array = np.array([var_array[0], var_array[1][1:]])
    msg = (
        "Only numpy arrays of variables, expressions, constraints and integers "
        "are accepted"
    )
    with pytest.raises(TypeError, match=msg):
        _ = facile.array(np_array)
