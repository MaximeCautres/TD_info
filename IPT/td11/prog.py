def decal(lettre, p):
    a = ord(lettre)
    return chr(ord('a') + (a-ord('a')+p)%26)

#print(decal('a', 3))

def cesar(word, p):
    w = ""
    for l in word:
        if l != ' ':
            w += decal(l, p)
        else:
            w += ' '
    return w

#print(cesar("erqvrlu ohv orxorx", -3))

def cesarsc(word):
    l = []
    for i in range(26):
        l += (i, cesar(word, -i))
    return l

#print(cesarsc('s uij jubbucudj vqsybu b ydvehcqjygku'))

def vigenere(text, clef):
    clef = clef.split()
    nclef = ""
    for i in clef:
        nclef += i
    i = -1
    w = ""
    for l in text:
        if l == " ":
            w += ' '
        else:
            i =  (i + 1)%len(nclef)
            w += decal(l, ord(nclef[i])-ord('a'))
    return w

#print(vigenere('bonsoit a tous les bestiaux', 'j aime le choux'))

def vigenereinverse(text, clef):
    clef = clef.split()
    nclef = ""
    for i in clef:
        nclef += i
    i = -1
    w = ""
    for l in text:
        if l == " ":
            w += ' '
        else:
            i =  (i + 1)%len(nclef)
            w += decal(l, -ord(nclef[i])+ord('a'))
    return w

#print(vigenereinverse('jsgnw qt txem', 'asm'))

def findcesaro(file):
    with open(file, "r", encoding="utf-8") as f:
        w = ""
        freq = [0 for _ in range(26)]
        for line in f.readline():
            for i in line:
                if 92 <= ord(i) <= 122:
                    w += i
                    freq[ord(i)-ord('a')] += 1
                else:
                    w += ' '
        print(freq)
        p = freq.index(max(freq))
        print(p)
        #return cesar(w, 4 -  p)
        return cesar(w, 5)
#print(findcesaro("txt1.txt"))

def exprapimod(m, e, n):
    if e == 0:
        return 1
    else:
        if e % 2 == 0:
            return exprapimod(m, e/2, n)**2 % n
        else:
            return (m*exprapimod(m, e//2, n)**2) % n

#print(exprapimod(10, 5, 85))

def euclide(a, b):
    if b == 0:
        return a
    else:
        return euclide(b, a % b)

#print(euclide(32, 27))

def euclide_entendu(a, b):
    if b == 0:
        return 0, -1
    else:
        a1, a2 = euclide_entendu(b, a % b)
        a3 = a // b
        return -a2 * a3, a1 + a2

print(euclide_entendu(7, 3))
