with open("input.txt") as input:
    inputList = [int(x) for x in input.read().splitlines()]
    
for i in inputList:
    if (2020 - i) in inputList:
        print(f"{i} * {2020 - i} = {i*(2020 - i)}")
        break
# part 1: 898299

for i in inputList:
    remainder = 2020 - i
    for j in inputList:
        if (remainder - j) in inputList:
            print(f"{i} * {j} * {remainder - j} = {i*j*(remainder-j)}")
            exit()
# part 2: 143933922