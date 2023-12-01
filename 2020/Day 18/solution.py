with open("input.txt") as input:
    inputList = input.readlines()

def parenthesize(statement):
    result = []
    currentList = result
    depth = 0
    for char in statement:
        if char == " " or char == "\n":
            continue
        if char == "(":
            depth += 1
            currentList.append([])
            currentList = currentList[-1]
        elif char == ")":
            depth -= 1
            currentList = result
            for _ in range(depth):
                currentList = currentList[-1]
        else:
            currentList.append(char)
    return result

def evaluate(statement):
    result = None
    nextop = None
    for part in statement:
        if type(part) == list:
            part = evaluate(part)
        if result == None:
            result = int(part)
        elif part == "*" or part == "+":
            nextop = part
        elif nextop == "*":
            result *= int(part)
        else:
            result += int(part)
    return result

def evaluate2(statement):
    for i, part in enumerate(statement):
        if type(part) == list:
            statement[i] = evaluate2(part)
    while len(statement) > 1:
        if statement.count("+") > 0:
            addIndex = statement.index("+")
            statement = statement[:addIndex-1] + [int(statement[addIndex-1]) + int(statement[addIndex+1])] + statement[addIndex+2:]
        else:
            mulIndex = statement.index("*")
            statement = statement[:mulIndex-1] + [int(statement[mulIndex-1]) * int(statement[mulIndex+1])] + statement[mulIndex+2:]
    return statement[0]

total = 0
for line in inputList:
    line = parenthesize(line)
    total += evaluate(line)
print(total)

total = 0
for line in inputList:
    line = parenthesize(line)
    total += evaluate2(line)
print(total)