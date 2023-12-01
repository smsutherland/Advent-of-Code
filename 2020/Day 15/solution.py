input = [0,1,5,10,3,12,19]

lastIndex = {}
prev = 0
for i in range(2020):
    new = 0
    if i < len(input):
        new = input[i]
    elif prev in lastIndex:
        new = i - lastIndex[prev] - 1
    if i:
        lastIndex[prev] = i - 1
    prev = new
print(prev)

for i in range(2020, 30000000):
    new = 0
    try:
        new = i - lastIndex[prev] - 1
    except: pass
    lastIndex[prev] = i - 1
    prev = new
print(prev)