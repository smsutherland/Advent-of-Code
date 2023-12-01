with open("input.txt") as input:
    inputList = input.readlines()

cubeGrid = {}
# keys: tuple of (x, y, z)
# vals: T/F on or off
for i, line in enumerate(inputList):
    for j, char in enumerate(line):
        cubeGrid[(j, i, 0)] = char == "#"

def getAdjacent(x, y, z):
    adj = []
    for i in range(-1, 2):
        for j in range(-1, 2):
            for k in range(-1, 2):
                if i != 0 or j != 0 or k != 0:
                    adj.append((x+i, y+j, z+k))
    return adj

def incDims(dims):
    return ((dims[0][0] - 1, dims[0][1] + 1), (dims[1][0] - 1, dims[1][1] + 1), (dims[2][0] - 1, dims[2][1] + 1))


def iteratecubeGrid(cubeGrid, dims):
    newCubegrid = {}
    dims = incDims(dims)
    for x in range(dims[0][0], dims[0][1]):
        for y in range(dims[1][0], dims[1][1]):
            for z in range(dims[2][0], dims[2][1]):
                adj = getAdjacent(x, y, z)
                numOn = 0
                for i in adj:
                    try:
                        numOn += cubeGrid[i]
                    except: pass
                if (x, y, z) in cubeGrid:
                    if cubeGrid[(x, y, z)]:
                        newCubegrid[(x, y, z)] = numOn == 2 or numOn == 3
                    else:
                        newCubegrid[(x, y, z)] = numOn == 3
                else:
                    newCubegrid[(x, y, z)] = numOn == 3
    return newCubegrid
                    



currentDims = ((0, len(inputList[0])), (0, len(inputList)), (0, 1))

for i in range(6):
    cubeGrid = iteratecubeGrid(cubeGrid, currentDims)
    currentDims = incDims(currentDims)

print(len([x for x in cubeGrid.values() if x]))





cubeGrid = {}
# keys: tuple of (x, y, z, w)
# vals: T/F on or off
for i, line in enumerate(inputList):
    for j, char in enumerate(line):
        cubeGrid[(j, i, 0, 0)] = char == "#"

def getAdjacent2(x, y, z, w):
    adj = []
    for i in range(-1, 2):
        for j in range(-1, 2):
            for k in range(-1, 2):
                for l in range(-1, 2):
                    if i != 0 or j != 0 or k != 0 or l != 0:
                        adj.append((x+i, y+j, z+k, w+l))
    return adj

def incDims2(dims):
    return ((dims[0][0] - 1, dims[0][1] + 1), (dims[1][0] - 1, dims[1][1] + 1), (dims[2][0] - 1, dims[2][1] + 1), (dims[3][0] - 1, dims[3][1] + 1))


def iteratecubeGrid2(cubeGrid, dims):
    newCubegrid = {}
    dims = incDims2(dims)
    for x in range(dims[0][0], dims[0][1]):
        for y in range(dims[1][0], dims[1][1]):
            for z in range(dims[2][0], dims[2][1]):
                for w in range(dims[3][0], dims[3][1]):
                    adj = getAdjacent2(x, y, z, w)
                    numOn = 0
                    for i in adj:
                        try:
                            numOn += cubeGrid[i]
                        except: pass
                    if (x, y, z, w) in cubeGrid:
                        if cubeGrid[(x, y, z, w)]:
                            newCubegrid[(x, y, z, w)] = numOn == 2 or numOn == 3
                        else:
                            newCubegrid[(x, y, z, w)] = numOn == 3
                    else:
                        newCubegrid[(x, y, z, w)] = numOn == 3
    return newCubegrid
                    



currentDims = ((0, len(inputList[0])), (0, len(inputList)), (0, 1), (0, 1))

for i in range(6):
    cubeGrid = iteratecubeGrid2(cubeGrid, currentDims)
    currentDims = incDims2(currentDims)

print(len([x for x in cubeGrid.values() if x]))
