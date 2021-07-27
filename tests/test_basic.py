import facile
import pytest


def test_simple() -> None:
    a = facile.variable(range(3))
    facile.constraint(a == 2)
    sol = facile.solve([a])
    assert sol is not None
    assert a.value() == 2


def test_overconstrained() -> None:
    a, b, c = [facile.variable(0, 1) for _ in range(3)]
    with pytest.raises(ValueError, match="The problem is overconstrained"):
        facile.constraint(facile.alldifferent([a, b, c]))
