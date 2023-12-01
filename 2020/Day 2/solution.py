with open("input.txt") as input:
    inputList = input.read().splitlines()

numValid1 = 0
numValid2 = 0
for line in inputList:
    policy, password = line.split(": ")
    a = policy.split(" ")
    policyNums = [int(x) for x in a[0].split("-")]
    policyLetter = a[1]
    passNum = password.count(policyLetter)
    if policyNums[0] <= passNum <= policyNums[1]:
        numValid1 += 1
    if (password[policyNums[0]-1] == policyLetter) ^ (password[policyNums[1]-1] == policyLetter):
        numValid2 += 1
print(numValid1)
print(numValid2)
# part 1: 591
# part 2: 335