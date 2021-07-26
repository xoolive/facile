from typing import Optional

from facile import Solution, constraint, minimize, variable


def coins(values, maxval) -> Optional[Solution]:
    """
    Which coins do you need to give back change for any amount between 0 and
    maxval, using coins from values?
    """

    # How many coin types
    n = len(values)
    nb_min_coins = [variable(range(maxval // values[i])) for i in range(n)]

    for val in range(maxval):
        # How many coins per type
        nb_coins = [variable(range(maxval // values[i])) for i in range(n)]
        mysum = sum([x[0] * x[1] for x in zip(values, nb_coins)])
        constraint(mysum == val)  # type: ignore
        for j in range(len(nb_coins)):
            constraint(nb_coins[j] <= nb_min_coins[j])

    total = variable(sum(nb_min_coins))  # type: ignore

    return minimize(nb_min_coins, total)


if __name__ == "__main__":
    sol = coins([1, 2, 5, 10, 20], 100)
    assert sol is not None
    for nb, val in zip(sol.solution, [1, 2, 5, 10, 20]):
        print("%d coin(s) of value %d" % (nb, val))
    print("Total of %d coins" % sol.evaluation)
