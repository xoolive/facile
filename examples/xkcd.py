from facile import *

#     http://xkcd.com/287/

#     Some amount (or none) of each dish should be ordered to give a total of
#     exact 15.05

price = [215, 275, 335, 355, 420, 580]
total = 1505

products = ["mixed fruit", "french fries", "side salad",
            "host wings", "mozzarella sticks", "samples place"]

# how many items of each dish
quantity = [variable(0, 10) for p in price]

constraint(total == sum(q * p for q, p in zip(quantity, price)))

res = solve_all(quantity, backtrack=True)

for r in res:
    if r.solution is None: # last solution
        print ("Total of {} backtracks".format(r.backtrack))
        break
    print ("Solution after {} backtracks:".format(r.backtrack))
    for i, x in enumerate(r.solution):
        if x > 0:
            print ("  {} × {:<20} : {:>5}".format(x, products[i], x*price[i]))
    total = sum(x*price[i] for i, x in enumerate(r.solution))
    print ("  Total                    : {:>5}".format(total))

