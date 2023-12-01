with open("input.txt") as input:
    inputList = input.read().splitlines()

slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
width = len(inputList[0])
total = 1
for slope in slopes:
    positionX = 0
    positionY = 0
    numTrees = 0
    while positionY < len(inputList):
        if inputList[positionY][positionX % width] == '#':
            numTrees += 1
        positionX += slope[0]
        positionY += slope[1]
    print(slope, numTrees)
    total *= numTrees
print(total)
# part 1: 184
# part 2: 2431272960