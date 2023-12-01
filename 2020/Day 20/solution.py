with open("input.txt") as input:
    inputList = input.read().split("\n\n")

tiles = {}
for tile in inputList:
    tile = tile.splitlines()
    number = int(tile[0][5:-1])
    tiles[number] = tile[1:]

for tile in tiles.values():
    for i, line in enumerate(tile):
        tile[i] = list(line)


def orientTile(tile, orientation):
    flipped = orientation > 3
    rotation = orientation%4
    if flipped:
        newTile = [None for x in range(len(tile))]
        for i, line in enumerate(tile):
            newTile[i] = line[::-1]
        tile = newTile
    
    for _ in range(rotation):
        newTile = [[None for y in range(len(tile[0]))] for x in range(len(tile))]
        for i in range(len(tile)):
            for j in range(len(tile[0])):
                newTile[j][len(tile[0])-1-i] = tile[i][j]
        tile = newTile
    return tile

matchingEdges = set()
# (a, b, c, d) means tile a matches with tile b if you orient b with c and align it with side d

unmatchedEdges = set()
# (a, b) means tile a's side b has no matching edges and therefore must be on the outside

for num1, tile1 in tiles.items():
    edgesFound = [False]*4
    for num2, tile2 in tiles.items():
        if num2 == num1:
            continue
        for i in range(8): # the number of orientations
            orientedTile = orientTile(tile2, i)
            
            if tile1[0] == orientedTile[-1]:
                matchingEdges.add((num1, num2, i, 0))
                edgesFound[0] = True
            if [x[-1] for x in tile1] == [x[0] for x in orientedTile]:
                matchingEdges.add((num1, num2, i, 1))
                edgesFound[1] = True
            if tile1[-1] == orientedTile[0]:
                matchingEdges.add((num1, num2, i, 2))
                edgesFound[2] = True
            if [x[0] for x in tile1] == [x[-1] for x in orientedTile]:
                matchingEdges.add((num1, num2, i, 3))
                edgesFound[3] = True
    for i in range(4):
        if not edgesFound[i]:
            unmatchedEdges.add((num1, i))

corners = set()
edges = set()
for edge in unmatchedEdges:
    num = edge[0]
    if num not in edges:
        edges.add(num)
    else:
        corners.add(num)
        edges.remove(num)

total = 1
for i in corners:
    total *= i
print(total)

seaMonster = """                  # 
#    ##    ##    ###
 #  #  #  #  #  #   """

corner = None
for i in corners:
    corner = i
    break

matchingEdgesDict = {}
for edge in matchingEdges:
    if edge[0] in matchingEdgesDict:
        matchingEdgesDict[edge[0]].append((edge[1], edge[2], edge[3]))
    else:
        matchingEdgesDict[edge[0]] = [(edge[1], edge[2], edge[3])]

rotationMap = [
[0, 1, 2, 3, 4, 5, 6, 7],
[1, 2, 3, 0, 7, 4, 5, 6],
[2, 3, 0, 1, 6, 7, 4, 5],
[3, 0, 1, 2, 5, 6, 7, 4],
[4, 5, 6, 7, 0, 1, 2, 3],
[5, 6, 7, 4, 3, 0, 1, 2],
[6, 7, 4, 5, 2, 3, 0, 1],
[7, 4, 5, 6, 1, 2, 3, 0]]

rotationSideMap = [
[0, 1, 2, 3],
[3, 0, 1, 2],
[2, 3, 0, 1],
[1, 2, 3, 0],
[0, 3, 2, 1],
[1, 0, 3, 2],
[2, 1, 0, 3],
[3, 2, 1, 0]]

tileOrder = [[(0, 0) for y in range(12)] for x in range(12)]



tileOrder[0][0] = (corner, 1)
for y in range(len(tileOrder)):
    for x in range(len(tileOrder[0])):
        if x == 0 and y == 0:
            continue
        
        prevX = x - 1
        prevY = y
        if x == 0:
            prevX = 0
            prevY = y - 1
        prevNum, prevRot = tileOrder[prevY][prevX]
        side = rotationSideMap[prevRot][1]
        if x == 0:
            side = rotationSideMap[prevRot][2]
        
        nextTile = None
        for possibility in matchingEdgesDict[prevNum]:
            if possibility[2] == side:
                nextTile = possibility
                break
        newRot = rotationMap[nextTile[1]][prevRot]
        tileOrder[y][x] = (nextTile[0], newRot)
    
# for i in tileOrder:
#     print(i)

def trimTile(tile):
    return [x[1:-1] for x in tile[1:-1]]

def trimTiles(tiles):
    for tileNum, tile in tiles.items():
        tiles[tileNum] = trimTile(tile)
    return tiles

def stitchTiles(tileMap, rotation):
    tileMap = orientTile(tileMap, rotation)
    global tiles
    brokenResult = [[None for y in range(12)] for x in range(12)]
    for y in range(12):
        for x in range(12):
            brokenResult[y][x] = orientTile(tiles[tileMap[y][x][0]], rotationMap[tileMap[y][x][1]][rotation])
    result = [[] for x in range(12*8)]
    for i in range(12*8):
        index = i%8
        tile = i//8
        for j in range(12):
            result[i].extend(brokenResult[tile][j][index])
    return result

def countMonsters(sea):
    global seaMonster
    seaMonsterList = seaMonster.splitlines()
    SMW = len(seaMonsterList[0])
    SMH = len(seaMonsterList)
    total = 0
    for y in range(len(sea) + 1 - SMH):
        for x in range(len(sea[y]) + 1 - SMW):
            area = [[None for x in range(SMW)] for y in range(SMH)]
            for i in range(len(area)):
                for j in range(len(area[i])):
                    area[i][j] = sea[y+i][x+j]
            for i in range(len(area)):
                for j in range(len(area[i])):
                    if seaMonsterList[i][j] == " ":
                        continue
                    if area[i][j] != "#":
                        break
                else:
                    continue
                break
            else:
                total += 1
    return total

def count2D(l, char):
    total = 0
    for l2 in l:
        total += l2.count(char)
    return total



tiles = trimTiles(tiles)

for i in range(8):
    sea = stitchTiles(tileOrder, i)
    # with open(f"{i}.txt", "w") as f:
    #     for line in result:
    #         for char in line:
    #             f.write(char)
    #         f.write("\n")

    if (num := countMonsters(sea)) > 0:
        print(count2D(sea, "#") - num*count2D(seaMonster, "#"))
        break

