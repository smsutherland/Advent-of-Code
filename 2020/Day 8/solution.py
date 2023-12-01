with open("input.txt") as input:
    inputList = input.read().splitlines()


lineNumber = 0
acc = 0
linesDone = []
while lineNumber not in linesDone:
    linesDone.append(lineNumber)
    line = inputList[lineNumber]
    op = line[:3]
    num = int(line[4:])
    if op == "acc":
        acc += num
    if op == "jmp":
        lineNumber += num - 1
    lineNumber += 1
print(acc)
# part 1: 1563

def executeCode(code):
    lineNumber = 0
    acc = 0
    linesDone = []
    while lineNumber not in linesDone:
        if lineNumber == len(inputList):
            print(acc)
            return True
        linesDone.append(lineNumber)
        line = code[lineNumber]
        op = line[:3]
        num = int(line[4:])
        if op == "acc":
            acc += num
            lineNumber += 1
        if op == "jmp":
            lineNumber += num
        if op == "nop":
            lineNumber += 1
    return False



for i in range(len(inputList)):
    line = inputList[i]
    op = line[:3]
    newInputList = [x for x in inputList]
    if op == "nop":
        newInputList[i] = "jmp " + line[4:]
    if op == "jmp":
        newInputList[i] = "nop " + line[4:]
    if executeCode(newInputList):
        break
# part 2: 767