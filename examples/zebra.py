from facile import *


colors = [variable(1, 5) for i in range(5)]
red, green, yellow, blue, ivory = colors

people = [variable(1, 5) for i in range(5)]
englishman, spaniard, japanese, ukrainian, norwegian = people

names = ["Englishman", "Spaniard", "Japanese", "Ukrainian", "Norwegian"]

animals = [variable(1, 5) for i in range(5)]
dog, snails, fox, zebra, horse = animals

drinks = [variable(1, 5) for i in range(5)]
tea, coffee, water, milk, fruit_juice = drinks

cigarettes = [variable(1, 5) for i in range(5)]
old_gold, kools, chesterfields, lucky_strike, parliaments = cigarettes

constraint(alldifferent(colors))
constraint(alldifferent(people))
constraint(alldifferent(animals))
constraint(alldifferent(drinks))
constraint(alldifferent(cigarettes))

constraint(englishman == red)
constraint(spaniard == dog)
constraint(coffee == green)
constraint(ukrainian == tea)
constraint(green == ivory + 1)
constraint(old_gold == snails)
constraint(kools == yellow)
constraint(milk == 3)
constraint(norwegian == 1)
constraint(abs(fox - chesterfields) == 1)
constraint(abs(horse - kools) == 1)
constraint(lucky_strike == fruit_juice)
constraint(japanese == parliaments)
constraint(abs(norwegian - blue) == 1)

assert solve(colors + people + animals + drinks + cigarettes)

water_drinker = [ n for n, p in zip(names, people) if p.value() == water.value() ]
zebra_owner = [ n for n, p in zip(names, people) if p.value() == zebra.value() ]

print("The {} drinks water.".format(water_drinker[0]))
print("The {} owns the zebra.".format(zebra_owner[0]))
