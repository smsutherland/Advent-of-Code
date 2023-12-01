with open("input.txt") as input:
    inputList = input.read().splitlines()
busses = [int(x) if x != "x" else 0 for x in inputList[1].split(",")]



def checkValid(busList, t):
    for i, bus in enumerate(busList):
        if bus != 0:
            if (t+i)%bus != 0:
                return False
    return True

def gcd(x, y):
    while y != 0:
        x, y = y, x % y
    return x

def lcm(x, y):
    lcm = x*y/gcd(x,y)
    return lcm


currentBusList = []
t = 1
currentTimeOffset = 1
for bus in busses:
    
    currentBusList.append(bus)
    if bus == 0:
        continue
    while True:
        if checkValid(currentBusList, t):
            # currentTimeOffset = lcm(currentTimeOffset, bus)
            currentTimeOffset *= bus
            break
        t += currentTimeOffset
print(t)



busses = [x for x in busses if x != 0]

startTime = int(inputList[0])
time = startTime
while True:
    for bus in busses:
        if time%bus == 0:
            print(bus*(time-startTime))
            break
    else:
        time += 1
        continue
    break