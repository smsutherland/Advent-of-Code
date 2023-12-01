import math

with open("input.txt") as input:
    inputList = input.read().splitlines()

x = 0
y = 0
deg = 0
i = complex(0, 1)
pos = complex(0, 0)
wpos = complex(10, 1)
for line in inputList:
    command = line[0]
    num = int(line[1:])
    if command == "N":
        y += num
        wpos += num*i
    elif command == "S":
        y -= num
        wpos -= num*i
    elif command == "E":
        x += num
        wpos += num
    elif command == "W":
        x -= num
        wpos -= num
    elif command == "L":
        deg += num
        wpos *= math.e**(i*math.pi*num/180)
    elif command == "R":
        deg -= num
        wpos *= math.e**(-i*math.pi*num/180)
    elif command == "F":
        x += num*math.cos(deg*math.pi/180)
        y += num*math.sin(deg*math.pi/180)
        pos += num*wpos
print(abs(x) + abs(y))
print(int(abs(pos.imag) + abs(pos.real)))