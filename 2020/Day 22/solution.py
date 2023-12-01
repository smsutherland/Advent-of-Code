import copy

with open("input.txt") as input:
    player1, player2 = input.read().split("\n\n")

player1 = [int(x) for x in player1.splitlines()[1:]]
player2 = [int(x) for x in player2.splitlines()[1:]]

while len(player1) != 0 and len(player2) != 0:
    player1Card = player1.pop(0)
    player2Card = player2.pop(0)
    winner = player1Card > player2Card
    if winner:
        player1.extend([player1Card, player2Card])
    else:
        player2.extend([player2Card, player1Card])

winner = None
if len(player1) > 0:
    winner = player1
else:
    winner = player2
score = 0
for i, card in enumerate(winner[::-1]):
    score += (i+1)*card
print(score)

with open("input.txt") as input:
    player1, player2 = input.read().split("\n\n")

player1 = [int(x) for x in player1.splitlines()[1:]]
player2 = [int(x) for x in player2.splitlines()[1:]]

results = []
depth = 0
def recursiveCombat(player1, player2):
    global depth
    global results
    prevRounds = set()
    while len(player1) != 0 and len(player2) != 0:
        if (str(player1), str(player2)) in prevRounds:
            results = player1
            return True
        prevRounds.add((str(player1), str(player2)))
        player1Card = player1.pop(0)
        player2Card = player2.pop(0)
        winner = player1Card > player2Card
        if len(player1) >= player1Card and len(player2) >= player2Card:
            winner = recursiveCombat(player1[:player1Card], player2[:player2Card])
        if winner:
            player1.append(player1Card)
            player1.append(player2Card)
        else:
            player2.append(player2Card)
            player2.append(player1Card)
    if len(player1) > 0:
        results = player1
    else:
        results = player2
    return len(player1) > 0

recursiveCombat(player1, player2)
score = 0
for i in range(len(results)):
    score += (len(results)-i)*results[i]
print(score)