cupOrder = [3,2,6,5,1,9,4,7,8]
# cupOrder = [3,8,9,1,2,5,4,6,7]
numCups = len(cupOrder)

currentCup = cupOrder[0]
for i in range(100):
    currentCupIndex = cupOrder.index(currentCup)
    next3 = [(currentCupIndex + 1)%numCups, (currentCupIndex + 2)%numCups, (currentCupIndex + 3)%numCups]
    
    for j, num in enumerate(next3):
        next3[j] = cupOrder[num]


    for k in next3:
        cupOrder.remove(k)
    
    j = -1
    destination = 0
    while True:
        if currentCup + j in cupOrder:
            destination = cupOrder.index(currentCup + j)
            break
        j -= 1
        if currentCup + j < min(cupOrder):
            j = max(cupOrder)
    cupOrder = cupOrder[:destination + 1] + next3 + cupOrder[destination+1:]
    currentCup = cupOrder[(cupOrder.index(currentCup)+1)%numCups]

print(cupOrder)
