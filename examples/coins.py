# -*- coding: utf-8 -*-

from facile import variable, constraint, minimize


def coins(values, maxval):
    """
    Which coins do you need to give back change for any amount between 0 and
    maxval, using coins from values?
    """

    # How many coin types
    nb_vals = len(values)
    nb_min_coins = [variable(0, maxval/values[i]) for i in range(nb_vals)]

    for val in range(maxval):
        # How many coins per type
        nb_coins = [variable(0, maxval/values[i]) for i in range(nb_vals)]
        mysum = sum([x[0] * x[1] for x in zip(values, nb_coins)])
        constraint(mysum == val)
        for j in range(len(nb_coins)):
            constraint(nb_coins[j] <= nb_min_coins[j])

    return minimize(nb_min_coins, sum(nb_min_coins))

if __name__ == "__main__":
    sol = coins([1, 2, 5, 10, 20], 100)
    for nb, val in zip(sol.solution, [1, 2, 5, 10, 20]):
        print ("%d coin(s) of value %d" % (nb, val))
    print ("Total of %d coins" % sol.evaluation)
