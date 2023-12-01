with open("input.txt") as input:
    inputList = input.read().splitlines()

foods = []
for line in inputList:
    ingredients, allergens = line.split("(")
    allergens = allergens[9:-1].split(", ")
    ingredients = ingredients.split()
    foods.append((ingredients, allergens))

allergenIngredients = {}
for ingredients, allergens in foods:
    for allergen in allergens:
        if allergen not in allergenIngredients:
            allergenIngredients[allergen] = set(ingredients)
        else:
            allergenIngredients[allergen] &= set(ingredients)

print(allergenIngredients)

allergenIngredientsFinal = {}
while [x for x in allergenIngredients.values() if len(x) != 1]:
# for _ in range(5):
    for allergen, ingredients in allergenIngredients.items():
        if allergen in allergenIngredientsFinal:
            continue
        otherRules = [x for y, x in allergenIngredients.items() if y != allergen]
        otherRulesIndices = set()
        for l in otherRules:
            otherRulesIndices |= set(l)
        
        toRemove = []
        for i in ingredients:
            if i in allergenIngredientsFinal.values():
                toRemove.append(i)
                continue
            if i not in otherRulesIndices:
                allergenIngredientsFinal[allergen] = i
                allergenIngredients[allergen] = [i]
                break
        else:
            for i in toRemove:
                allergenIngredients[allergen].discard(i)
        if len(ingredients) == 1:

            allergenIngredientsFinal[allergen] = list(ingredients)[0]

total = 0
for ingredients, _ in foods:
    for i in ingredients:
        if i not in allergenIngredientsFinal.values():
            total += 1
print(total)

allergens = list(allergenIngredientsFinal.keys())
allergens.sort()
finalstr = ""
for a in allergens:
    finalstr += allergenIngredientsFinal[a] + ","
print(finalstr[:-1])