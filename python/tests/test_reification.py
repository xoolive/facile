import facile


def test_magical() -> None:
    array = [facile.variable(range(10)) for i in range(10)]

    for i in range(10):
        sum_ = facile.sum(x == i for x in array)
        facile.constraint(sum_ == array[i])

    solution = facile.solve(array)

    assert solution.solved
    assert solution.solution == [6, 2, 1, 0, 0, 0, 1, 0, 0, 0]
