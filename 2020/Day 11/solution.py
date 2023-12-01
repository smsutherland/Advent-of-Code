with open("input.txt") as input:
    inputList = input.read().splitlines()

# inputList = """L.LL.LL.LL
# LLLLLLL.LL
# L.L.L..L..
# LLLL.LL.LL
# L.LL.LL.LL
# L.LLLLL.LL
# ..L.L.....
# LLLLLLLLLL
# L.LLLLLL.L
# L.LLLLL.LL"""
# inputList = inputList.splitlines()

width = len(inputList[0])
height = len(inputList)

def genEmptyList():
    newList = []
    for j in range(height):
        newList.append([])
        for i in range(width):
            newList[j].append(Seat("."))
    return newList

class Seat:
    def __init__(self, char):
        self.floor = True
        self.taken = False
        self.char = char
        if char == "L":
            self.floor = False
        if char == "#":
            self.floor = False
            self.taken = True
    
    def equals(self, seat2):
        return (self.floor == seat2.floor) and (self.taken == seat2.taken)

    def getChar(self):
        return self.char

def getAdjacent(list, i, j):
    adjacent = []
    for x in [-1, 0, 1]:
        for y in [-1, 0, 1]:
            if 0 <= i + x < width:
                if 0 <= j + y < height:
                    if x != 0 or y != 0:
                        adjacent.append(list[j + y][i + x])
    return adjacent

def runSeats(seatChart):
    newList = deepcopy(seatChart)
    for i in range(width):
        for j in range(height):
            adjacentSeats = getAdjacent(seatChart, i, j)
            numTaken = len([x for x in adjacentSeats if x.taken])
            if seatChart[j][i].getChar() == "L" and numTaken == 0:
                newList[j][i] = Seat("#")
            if seatChart[j][i].getChar() == "#" and numTaken >= 4:
                newList[j][i] = Seat("L")
    return newList

def checkEqual(chart1, chart2):
    if len(chart1) != len(chart2):
        return False
    for i in range(width):
        for j in range(height):
            if not (chart1[j][i].equals(chart2[j][i])):
                return False
    return True

def deepcopy(chart):
    newList = genEmptyList()
    for i in range(width):
        for j in range(height):
            newList[j][i] = Seat(chart[j][i].getChar())
    return newList

def printChart(chart):
    for y in range(height):
        line = ""
        for x in range(width):
            line += chart[y][x].getChar()
        print(line)
    print()

seatChart = []
for j in range(height):
    seatChart.append([])
    for i in range(width):
        seatChart[j].append(Seat(inputList[j][i]))

prevList = []
numTimes = 0
while not checkEqual(prevList, seatChart):
    numTimes += 1
    prevList = deepcopy(seatChart)
    seatChart = runSeats(seatChart)

numTaken = 0
for i in range(width):
    for j in range(height):
        if seatChart[j][i].taken:
            numTaken += 1
print(numTaken)

#part 2 begins

def getAdjacent2(list, i, j):
    adjacent = []
    d = 0
    found = [False]*8
    while False in found:
        d += 1

        if i-d < 0:
            found[0] = True
        elif not list[j][i-d].floor and not found[0]:
            adjacent.append(Seat(list[j][i-d].getChar()))
            found[0] = True
        
        if (i-d < 0) or (j-d < 0):
            found[1] = True
        elif not list[j-d][i-d].floor and not found[1]:
            adjacent.append(Seat(list[j-d][i-d].getChar()))
            found[1] = True
        
        if j-d < 0:
            found[2] = True
        elif not list[j-d][i].floor and not found[2]:
            adjacent.append(Seat(list[j-d][i].getChar()))
            found[2] = True
        
        if (i+d >= width) or (j-d < 0):
            found[3] = True
        elif not list[j-d][i+d].floor and not found[3]:
            adjacent.append(Seat(list[j-d][i+d].getChar()))
            found[3] = True
        
        if i+d >= width:
            found[4] = True
        elif not list[j][i+d].floor and not found[4]:
            adjacent.append(Seat(list[j][i+d].getChar()))
            found[4] = True
        
        if (i+d >= width) or (j+d >= height):
            found[5] = True
        elif not list[j+d][i+d].floor and not found[5]:
            adjacent.append(Seat(list[j+d][i+d].getChar()))
            found[5] = True
        
        if j+d >= height:
            found[6] = True
        elif not list[j+d][i].floor and not found[6]:
            adjacent.append(Seat(list[j+d][i].getChar()))
            found[6] = True
        
        if (i-d < 0) or (j+d >= height):
            found[7] = True
        elif not list[j+d][i-d].floor and not found[7]:
            adjacent.append(Seat(list[j+d][i-d].getChar()))
            found[7] = True
        
    return adjacent

def runSeats2(seatChart):
    newList = deepcopy(seatChart)
    for i in range(width):
        for j in range(height):
            adjacentSeats = getAdjacent2(seatChart, i, j)
            numTaken = len([x for x in adjacentSeats if x.taken])
            if seatChart[j][i].getChar() == "L" and numTaken == 0:
                newList[j][i] = Seat("#")
            if seatChart[j][i].getChar() == "#" and numTaken >= 5:
                newList[j][i] = Seat("L")
    return newList

seatChart = []
for j in range(height):
    seatChart.append([])
    for i in range(width):
        seatChart[j].append(Seat(inputList[j][i]))

prevList = []
numTimes = 0
while not checkEqual(prevList, seatChart):
    numTimes += 1
    prevList = deepcopy(seatChart)
    seatChart = runSeats2(seatChart)
    printChart(seatChart)

numTaken = 0
for i in range(width):
    for j in range(height):
        if seatChart[j][i].taken:
            numTaken += 1
print(numTaken)