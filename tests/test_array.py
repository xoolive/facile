import facile


def test_basic_array() -> None:
    array = facile.array([facile.variable(0, 1) for _ in range(5)])
    x = facile.variable(range(10))
    facile.constraint(array[x] == 1)
    facile.constraint(array.sum() == 1)
    facile.constraint(x == 1)

    solution = facile.solve([*array, x])
    assert solution.solved
    assert array.value() == [0, 1, 0, 0, 0]
