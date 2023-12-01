with open("input.txt") as input:
    ruleList, inputList = input.read().split("\n\n")

def parseRules(rules):
    parsedRules = {}
    for rule in rules:
        num, rule = rule.split(": ")
        if '"' in rule:
            rule = rule[1]
            parsedRules[int(num)] = rule
        else:
            options = rule.split(" | ")
            for i, option in enumerate(options):
                options[i] = [int(x) for x in option.split()]
            parsedRules[int(num)] = options
    return parsedRules

rules = parseRules(ruleList.splitlines())

def followsRule(line, ruleNum):
    global rules
    thisRule = rules[ruleNum]
    if type(thisRule) == str:
        if len(line) > 0:
            if line[0] == thisRule:
                return (True, 1)
            else:
                return (False, 0)
        else:
            return (False, 0)
    for ruleSet in thisRule:
        numRemoved = 0
        for rule in ruleSet:
            result = followsRule(line[numRemoved:], rule)
            if result[0]:
                numRemoved += result[1]
            else:
                break
        else:
            return (True, numRemoved)
    return (False, 0)

def matchesZero(line):
    result = followsRule(line, 0)
    result = result[1] == len(line) and result[0]
    return result

def matchesZero2(line):
    numRemoved = 0
    num42 = 0
    num31 = 0
    while True:
        result = followsRule(line[numRemoved:], 42)
        if result[0]:
            numRemoved += result[1]
            num42 += 1
        else:
            break
    while True:
        result = followsRule(line[numRemoved:], 31)
        if result[0]:
            numRemoved += result[1]
            num31 += 1
        else:
            break
    if len(line) == numRemoved and num42 > num31 and num31 > 0:
        return True

print(len([True for x in inputList.splitlines() if matchesZero(x)]))
print(len([True for x in inputList.splitlines() if matchesZero2(x)]))
