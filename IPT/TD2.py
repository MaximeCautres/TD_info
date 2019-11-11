from time import time
import random
import math

def factoriel(n):
    if n == 0:
        return 1
    else:
        return n*factoriel(n-1)

def fact2(n):
    s = 1
    for i in range(n):
        s*=i
    return s

def compar():
    l = []
    for _ in range(100):
        n = random.randrange(0,101)
        l+=[[n]]
        debut = time()
        factoriel(n)
        fin = time()
        t = fin-debut
        l[-1].append(t)
        debut2 = time()
        fact2(n)
        fin2 = time()
        t_p = fin2 - debut2
        l[-1].append(t_p)
    print(l)
    return


def pgcd(a,b):
    if b==0:
        return a
    else:
        return pgcd(b,a%b)

def recursive(n, uz, f):
    if n == 0:
        return uz
    else:
        return f(recursive(n-1,uz ,f))

#print(recursive(2,2,lambda x : x + 2))

def iterative(n ,uz, f):
    for _ in range(n):
        uz = f(uz)
    return uz

def summa(l):
    if l == []:
        return 0
    else:
        return l[0] + summa(l[1:])

#print(summa([2,3,4]))

def summamoche(l):
    s = 0
    for i in l:
        s += i
    return s

def fusion(l1,l2):
    if l1==[]:
        return l2
    if l2==[]:
        return l1
    if l1[0] < l2[0]:
        return [l1[0]]+fusion(l1[1:],l2)
    else:
        return [l2[0]]+fusion(l1, l2[1:])

#print(fusion([1,3,5],[4,6]))

def puissance(x, n):
    if n == 0:
        return 1
    else:
        return x * puissance(x, n-1)

def puissancenew(x, n):
    if n == 0:
        return 1
    elif n % 2 == 0:
        return puissancenew(x, n/2)**2
    else:
        return 2*puissancenew(x,n//2)**2

def binom(n, p):
    if p==n or p==0:
        return 1
    else:
        return binom(n-1, p)+binom(n-1, p-1)

#print(binom(6, 3))

def triangle(n):
    n+=1
    l = [[0 for _ in range(n)] for  _ in range(n)]
    for i in range(n):
        l[i][0]=1
    for i in range(1,n):
        for j in range(1, n):
            l[i][j]=l[i-1][j]+l[i-1][j-1]
    for i in l:
        print(i)

#triangle(6)

def anagramme(str):
    l = []
    for i in str:
        if l == []:
            l+=[i]
        else:
            l_p = []
            for anml in l:
                for c in range(len(anml)+1):
                    l_p.append(anml[:c]+i+anml[c:])
            l = l_p[:]
    return l

def catalan(n):
    if n == 0:
        return 1
    else:
        s = 0
        for i in range(0, n):
            s += catalan(i)*catalan(n-1-i)
        return s

#print(catalan(6))

def catalanmemo(n):
    l = [0 for _ in range(n+1)]
    l[0]=1
    for i in range(1,n+1):
        for j in range(0,i):
            l[i]+= l[j]*l[i-1-j]
    return l[-1]

#print(catalanmemo(5))

a = 2
b = 5

def u(n):
    if n == 0:
        return a
    else:
        return (u(n-1) + v(n-1))/2

def v(n):
    if n == 0:
        return b
    else:
        return math.sqrt(u(n-1)*v(n-1))

#print(u(20))

def cross(n):
    if n == 0:
        return a, b
    else:
        u_p, v_p = cross(n-1)
        return (u_p + v_p)/2, math.sqrt(u_p*v_p)

#print(cross(20))

def crossmoche(n):
    u=a, v = b
    for i in range(n):
        u, v = (u + v)/2, math.sqrt(u*v)
    return u, v

def testpar(e):
    s = ""
    for i in e:
        if i =='(' or i == ')':
            s+=i
    c = 0
    while c != -1 and c!= len(s) and s != "":
        if s[c:c+2] == '()':
            s = s[:c]+ s[c+2:]
        elif s[c:c+2] == '((':
            c+=1
        elif s[c] == ')':
            c-=1
        elif c == len(s)-1 and s[c]=='(':
            c=-1
        print(s,c)
    return s == ""

print(testpar("((1+2)*3+(4*((5-6)+7)-8*9))"))

"""exercice 11"""

# Deja effectue dans le TD3 en utilisant des piles

"""exercice 12"""


def creerFile():
    return []


def enfiler(elt, f):
    return f.append(elt)


def defiler(f):
    x = f[-1]
    f = f[:-1]
    return x

def estFileVide(f):
    return f == []

def longueur(f):
    return len(f)

def affiche(f):
    print(f)
    return


# f = creerFile()
# f = enfiler(1, f)
# affiche(f)
# f = enfiler(2, f)
# f = enfiler(3, f)
# f = enfiler(4, f)
# affiche(f)
# longueur(f)
# print(defile(f))
# affiche(f)


"""exercice 13 partition d'entree"""

