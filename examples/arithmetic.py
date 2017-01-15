import re
from functools import reduce

import facile as fcl


def arithmetic(puzzle="SEND+MORE=MONEY", base=10):

    problem = re.split("[\s+=]", puzzle)

    # remove spaces
    problem = list(filter(lambda w: len(w) > 0, problem))

    # letters
    letters = {l: fcl.variable(0, base - 1) for l in set("".join(problem))}

    # expressions
    expr_pb = [[letters[a] for a in word] for word in problem]
    words = [reduce(lambda a, b: 10 * a + b, word) for word in expr_pb]

    # constraints
    fcl.constraint(fcl.alldifferent(letters.values()))
    fcl.constraint(sum(words[:-1]) == words[-1])

    for word in expr_pb: fcl.constraint(word[0] > 0)

    assert fcl.solve(letters.values())

    # print solutions
    for word, numbers in zip(problem, expr_pb):
        strings = [str(n.value()) for n in numbers]
        print(word + " = " + "".join(strings))


if __name__ == '__main__':

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
