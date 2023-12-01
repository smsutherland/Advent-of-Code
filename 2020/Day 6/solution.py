with open("input.txt") as input:
    inputList = input.read().split("\n\n")

total1 = 0
total2 = 0
for group in inputList:
    people = group.splitlines()
    groupQuestions1 = set(people[0])
    groupQuestions2 = set(people[0])
    for person in people[1:]:
        personSet = set(person)
        groupQuestions1 |= personSet
        groupQuestions2 &= personSet
    total1 += len(groupQuestions1)
    total2 += len(groupQuestions2)
print(total1)
print(total2)
# part 1: 6947
# part 2: 3398