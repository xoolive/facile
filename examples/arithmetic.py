import re
from functools import reduce
from typing import Union

import facile

Expression = Union[facile.Variable, facile.Arith]


def arithmetic(puzzle="SEND+MORE=MONEY", base=10) -> None:

    problem = re.split(r"[\s+=]", puzzle)

    # remove spaces
    problem = list(filter(lambda w: len(w) > 0, problem))

    letters = {
        letter: facile.variable(range(base)) for letter in set("".join(problem))
    }

    # expressions
    expr_pb = [[letters[a] for a in word] for word in problem]

    def horner(a: Expression, b: Expression):
        return 10 * a + b

    words = [reduce(horner, word) for word in expr_pb]

    # constraints
    facile.constraint(facile.alldifferent(letters.values()))
    facile.constraint(facile.sum(words[:-1]) == words[-1])

    for word in expr_pb:
        facile.constraint(word[0] > 0)

    assert facile.solve(letters.values())

    # print solutions
    for word, numbers in zip(problem, expr_pb):
        strings = [str(n.value()) for n in numbers]
        print(f"{word} = {''.join(strings)}")


if __name__ == "__main__":

    problems = [
        "SEND + MORE = MONEY",
        "SEND + MOST = MONEY",
        "VINGT + CINQ + CINQ = TRENTE",
        "EIN + EIN + EIN + EIN = VIER",
        "DONALD + GERALD = ROBERT",
        "SATURN + URANUS + NEPTUNE + PLUTO = PLANETS",
        "TIGRE + LIONNE = TIGRON",
        "WRONG + WRONG = RIGHT",
    ]

    for p in problems:
        arithmetic(p)
        print()  # new line!
