with open("input.txt") as input:
    inputList = input.read().splitlines()


bags = {}
for line in inputList:
    line = line[:-1]
    container, contains = line.split(" contain ")
    contains = contains.split(", ")
    if contains == ["no other bags"]:
        contains = []
    for i in range(len(contains)):
        if contains[i][-1] != 's':
            contains[i] += "s"
        
    bags[container] = contains

def canContain(bag):
    for containedBag in bags[bag]:
        if containedBag[2:] == "shiny gold bags":
            return True
    for nextBag in bags[bag]:
        if canContain(nextBag[2:]):
            return True
    return False

def numContained(bag):
    contained = bags[bag]
    total = 0
    for i in contained:
        num = int(i[0:1])
        bagName = i[2:]
        total += num * (numContained(bagName)+1)
    return total

print(len([x for x in bags.keys() if canContain(x)]))
# part 1: 233

print(numContained("shiny gold bags"))
# part 2: 421550