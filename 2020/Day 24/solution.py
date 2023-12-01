with open("input.txt") as input:
    inputList = input.read().splitlines()

# inputList = """sesenwnenenewseeswwswswwnenewsewsw
# neeenesenwnwwswnenewnwwsewnenwseswesw
# seswneswswsenwwnwse
# nwnwneseeswswnenewneswwnewseswneseene
# swweswneswnenwsewnwneneseenw
# eesenwseswswnenwswnwnwsewwnwsene
# sewnenenenesenwsewnenwwwse
# wenwwweseeeweswwwnwwe
# wsweesenenewnwwnwsenewsenwwsesesenwne
# neeswseenwwswnwswswnw
# nenwswwsewswnenenewsenwsenwnesesenew
# enewnwewneswsewnwswenweswnenwsenwsw
# sweneswneswneneenwnewenewwneswswnese
# swwesenesewenwneswnwwneseswwne
# enesenwswwswneneswsenwnewswseenwsese
# wnwnesenesenenwwnenwsewesewsesesew
# nenewswnwewswnenesenwnesewesw
# eneswnwswnwsenenwnwnwwseeswneewsenese
# neswnwewnwnwseenwseesewsenwsweewe
# wseweeenwnesenwwwswnew""".splitlines()
# inputList = ["wseweeenwnesenwwwswnew"]

directions = ['e', 'se', 'sw', 'w', 'nw', 'ne']
opposites = {'e': 'w', 'w': 'e', 'se': 'nw' ,'nw': 'se', 'sw': 'ne', 'ne': 'sw'}
# reductionsNE = [('se', 'e'), ('w', 'nw')]
# reductionsSW = [('nw', 'w'), ('e', 'se')]


tiles = []
for line in inputList:
    lineArray = []
    buffer = ''
    for char in line:
        if buffer != '':
            lineArray.append(buffer + char)
            buffer = ''
        elif char in directions:
            lineArray.append(char)
        else:
            buffer = char
    tiles.append(lineArray)

for tile in tiles:
    while True:
        for direction in tile:
            if opposites[direction] in tile:
                tile.remove(opposites[direction])
                tile.remove(direction)
                break
            if direction == 'ne':
                tile.remove('ne')
                tile.append('e')
                tile.append('nw')
                break
            if direction == 'sw':
                tile.remove('sw')
                tile.append('w')
                tile.append('se')
                break
        else:
            break
    tile.sort()

finalTileList = []
for tile in tiles:
    if tile not in finalTileList:
        finalTileList.append(tile)
    else:
        finalTileList.remove(tile)
print(len(finalTileList))

tileCoords = []
for tile in finalTileList:
    coords = [0, 0]
    coords[0] += tile.count('se')
    coords[0] -= tile.count('nw')
    coords[1] += tile.count('e')
    coords[1] -= tile.count('w')
    tileCoords.append(tuple(coords))

def getAdj(coords):
    x = coords[0]
    y = coords[1]
    return [(x-1, y), (x-1, y+1), (x, y+1), (x+1, y), (x+1, y-1), (x, y-1)]

for i in range(100):
    print(i+1)
    newTiles = []
    testTiles = []
    for tile in tileCoords:
        adj = getAdj(tile)
        if len([True for x in adj if x in tileCoords]) == 1:
            newTiles.append(tile)
        testTiles.extend(adj)
    testTilesShort = []
    [testTilesShort.append(x) for x in testTiles if x not in testTilesShort]
    for tile in testTilesShort:
        if len([True for x in getAdj(tile) if x in tileCoords]) == 2:
            newTiles.append(tile)
    tileCoords = newTiles

print(len(tileCoords))