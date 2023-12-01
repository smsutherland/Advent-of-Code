with open("input.txt") as input:
    inputList = input.read().split("\n\n")

neededCreds = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

numValid1 = 0
numValid2 = 0
for passport in inputList:
    creds = passport.split()

    gottenCreds = {}
    for cred in creds:
        credName = cred[0:3]
        if credName in neededCreds:
            gottenCreds[credName] = cred[4:]
        
    valid = True
    for credName in neededCreds:
        if credName not in gottenCreds.keys():
            valid = False
            break
    else:
        numValid1 += 1
    
    for credName, cred in gottenCreds.items():
        if not valid:
            break

        if credName == "byr":
            if len(cred) != 4:
                valid = False
            if not (1920 <= int(cred) <= 2002):
                valid = False

        if credName == "iyr":
            if len(cred) != 4:
                valid = False
            if not (2010 <= int(cred) <= 2020):
                valid = False

        if credName == "eyr":
            if len(cred) != 4:
                valid = False
            if not (2020 <= int(cred) <= 2030):
                valid = False

        if credName == "hgt":
            if cred[-2:] == "cm":
                if not (150 <= int(cred[:-2]) <= 193):
                    valid = False
            elif cred[-2:] == "in":
                if not (59 <= int(cred[:-2]) <= 76):
                    valid = False
            else:
                valid = False

        if credName == "hcl":
            if cred[0] != "#":
                valid = False
            for letter in cred[1:]:
                if letter not in "0123456789abcdef":
                    valid = False

        if credName == "ecl":
            if cred not in ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]:
                valid = False

        if credName == "pid":
            if len(cred) != 9:
                valid = False
            for letter in cred:
                if letter not in "0123456789":
                    valid = False
                    
    if valid:
        numValid2 += 1
print(numValid1)
print(numValid2)
# part 1: 202
# part 2: 137