import facile
import pytest


def test_solution() -> None:
    a = facile.variable([0, 1])
    b = facile.variable([0, 1])
    facile.constraint(a != b)
    sol = facile.solve([a, b])
    assert sol.solved


def test_nosolution() -> None:
    a, b, c = [facile.variable(0, 1) for _ in range(3)]
    facile.constraint(a != b)
    facile.constraint(b != c)
    facile.constraint(c != a)
    sol = facile.solve([a, b, c])
    assert not sol.solved


def test_overconstrained() -> None:
    a, b, c = [facile.variable(0, 1) for _ in range(3)]
    with pytest.raises(ValueError, match="The problem is overconstrained"):
        facile.constraint(facile.alldifferent([a, b, c]))


def test_domains() -> None:
    a = facile.variable(range(320))
    b = facile.variable(range(160))
    c = facile.variable(range(130))
    d = facile.variable(range(130))

    facile.constraint(a + b + c + d == 711)
    assert len(a.domain()) == 26
    assert len(b.domain()) == 26
    assert len(c.domain()) == 26
    assert len(d.domain()) == 26

    facile.constraint(a * b * c * d == 711000000)
    assert len(a.domain()) == 17
    assert len(b.domain()) == 23
    assert len(c.domain()) == 20
    assert len(d.domain()) == 20

    sol = facile.solve([a, b, c, d], backtrack=True)
    assert sol.solved
    assert sol.backtrack == 2
