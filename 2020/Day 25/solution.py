with open("input.txt") as input:
    publicKeys = [int(x) for x in input.read().splitlines()]
# publicKeys = [17807724, 5764801]
itrs = [0, 0]
num = 1
i = 1
while True:
# for z in range(10):
    # print(num)
    num *= 7
    num %= 20201227
    if num in publicKeys:
        itrs[publicKeys.index(num)] = i
    if 0 not in itrs:
        break
    i += 1
print(itrs)
num = 1
for i in range(min(itrs)):
    num *= publicKeys[itrs.index(max(itrs))]
    num %= 20201227
print(num)