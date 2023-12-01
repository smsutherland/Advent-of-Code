with open("input.txt") as input:
    inputList = input.read().splitlines()

max = 0
idList = []
for line in inputList:
    id = 0
    for i in range(10):
        if line[i] in "BR":
            id += 2**(9-i)

    if id > max:
        max = id
    idList.append(id)
print(max)
# part 1: 919

seat = [x for x in range(max) if ((x-1) in idList) and ((x+1) in idList) and x not in idList][0]
print(seat)
# part 2: 642
