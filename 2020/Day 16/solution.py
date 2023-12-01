with open("input.txt") as input:
    inputList = input.read().split("\n\n")

# input = """class: 0-1 or 4-19
# row: 0-5 or 8-19
# seat: 0-13 or 16-19

# your ticket:
# 11,12,13

# nearby tickets:
# 3,9,18
# 15,1,5
# 5,14,9"""
# inputList = input.split("\n\n")

rulesList = inputList[0].splitlines()
myTicket = inputList[1].splitlines()[1]
nearbyTickets = inputList[2].splitlines()[1:]

#process rules
rules = {}
for rule in rulesList:
    name, limits = rule.split(": ")
    limit1, limit2 = limits.split(" or ")
    limit1 = limit1.split("-")
    limit2 = limit2.split("-")
    valid = set(range(int(limit1[0]), int(limit1[1]) + 1)).union(set(range(int(limit2[0]), int(limit2[1]) + 1)))
    rules[name] = valid

allValid = set()
for i in rules.values():
    allValid |= i

totals = []
for i in range(len(nearbyTickets[0].split(','))):
    totals.append([])

total = 0
for ticket in nearbyTickets:
    nums = ticket.split(',')

    notValid = [int(x) for x in nums if int(x) not in allValid]
    error = sum(notValid)
    if len(notValid) == 0:
        for i in range(len(totals)):
            totals[i].append(nums[i])

    total += error

myTicket = [int(x) for x in myTicket.split(",")]
for i, val in enumerate(myTicket):
    totals[i].append(val)

print(total)

ruleIndices = {}
for ruleName in rules:
    ruleIndices[ruleName] = []

for ruleName, rule in rules.items():
    for i, field in enumerate(totals):
        if [x for x in field if int(x) not in rule]:
            continue
        ruleIndices[ruleName].append(i)

ruleIndicesFinal = {}
while [x for x in ruleIndices.values() if len(x) != 1]:
    for ruleName, ruleIndexList in ruleIndices.items():
        if ruleName in ruleIndicesFinal:
            continue
        otherRules = [x for y, x in ruleIndices.items() if y != ruleName]
        otherRulesIndices = set()
        for l in otherRules:
            otherRulesIndices |= set(l)
        
        toRemove = []
        for i in ruleIndexList:
            if i in ruleIndicesFinal.values():
                toRemove.append(i)
                continue
            if i not in otherRulesIndices:
                ruleIndicesFinal[ruleName] = i
                ruleIndices[ruleName] = [i]

                break
        else:
            for i in toRemove:
                ruleIndices[ruleName].remove(i)
        if len(ruleIndexList) == 1:
            ruleIndicesFinal[ruleName] = ruleIndexList[0]




ticketNum = 1
for rule in ruleIndicesFinal:
    if rule.startswith("departure"):
        index = ruleIndicesFinal[rule]
        ticketNum *= myTicket[index]

print(ticketNum)