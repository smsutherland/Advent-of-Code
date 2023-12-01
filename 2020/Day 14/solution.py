with open("input.txt") as input:
    inputList = input.read().splitlines()

def binary(x):
    num = bin(x)[2:]
    while len(num) < 36:
        num = "0" + num
    return num

def decimal(x):
    num = 0
    for i, char in enumerate(x):
        num += int(char)*2**(len(x) - i - 1)
    return num

def replaceIndex(s, i, r):
    return s[:i] + r + s[i+1:]

mask = "X"*36
memory = {}

for line in inputList:
    command, param = line.split(" = ")
    if command == "mask":
        mask = param
        continue
    else:
        address = int(command[4:-1])
        param = binary(int(param))
        num = ""
        for i, char in enumerate(mask):
            if char == "X":
                num += param[i]
            else:
                num += char
        memory[address] = num

total = 0
for num in memory.values():
    total += decimal(num)
print(total)

mask = "0"*36
memory = {}

for line in inputList:
    command, param = line.split(" = ")
    if command == "mask":
        mask = param
        continue
    else:
        address = binary(int(command[4:-1]))
        param = int(param)
        for i, char in enumerate(mask):
            if char == "X":
                address = replaceIndex(address, i, "X")
            elif char == "1":
                address = replaceIndex(address, i, "1")
        
        for i in range(2**address.count("X")):
            binar = binary(i)
            binIndex = 1
            newAddress = address
            for j, char in enumerate(address):
                if char == "X":
                    newAddress = replaceIndex(newAddress, j, binar[-binIndex])
                    binIndex += 1
            memory[newAddress] = param

total = 0
for num in memory.values():
    total += num
print(total)