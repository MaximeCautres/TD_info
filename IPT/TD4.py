import random


def est_trie(l):
    if len(l) == 1:
        return True
    else:
        if l[0] <= l[1]:
            return est_trie(l[1:])
        else:
            return False


def liste_aleatoire(n, M):
    return [random.randrange(M + 1) for _ in range(n)]


def tri_insertion(l):
    for i in range(1, len(l)):
        k = i
        while l[k-1] > l[k] and k > 0:
            l[k-1], l[k] = l[k], l[k-1]
            k -= 1
    return l

#print(tri_insertion([9, 8, 7, 6, 5, 4, 4, 4, 5, 5,4]))

def fusion (l1, l2):
    if l1 == [] or l2 == []:
        return l1 + l2
    if l1[0] <= l2[0]:
        return [l1[0]]+fusion(l1[1:], l2)
    else:
        return [l2[0]]+fusion(l1, l2[1:])


def tri_fusion(l):
    if len(l) == 1:
        return l
    else:
        return fusion(tri_fusion(l[:len(l)//2]), tri_fusion(l[len(l)//2:]))

#print(tri_fusion([9, 8, 7, 6, 5, 4, 4, 4, 5, 5,4]))


def tri_rapide(l):
    if len(l) <= 1:
        return l
    else:
        l1, l2= [], []
        for i in l[1:]:
            if i < l[0]:
                l1.append(i)
            else:
                l2.append(i)
        return tri_rapide(l1) + [l[0]] + tri_rapide(l2)

#print(tri_rapide([9, 8, 7, 6, 5, 4, 4, 4, 5, 5,4]))

def selection(l):
    for i in reversed(range(1, len(l))):
        max = l[0]
        indice = 0
        for j in range(0, i):
            if l[j] > max:
                max = l[j]
                indice = j
        l[indice], l[i] = l[i], l[indice]
    return l

#print(selection([9, 8, 7, 6, 5, 4, 4, 4, 5, 5,4]))

def fusioniter(l1, l2):
    c1 = 0
    c2 = 0
    l = []
    while c1 != len(l1) - 1 and c2 != len(l2) - 1:
        if l1[c1] <= l2[c2]:
            l.append(l1[c1])
            c1 += 1
        else:
            l.append(l2[c2])
            c2 += 1
    return l

#print(fusion([1,2,3,4], [2,3,4,5,6]))

"""def mediane(l):
    l = {}
    for i in l:
"""


def unique(l):
    l_p = tri_fusion(l)
    l = [l_p[0]]
    for i in range(1, len(l_p)):
        if l_p[i] != l[-1]:
            l.append(l_p[i])
    return l


#print(unique([9, 8, 7, 6, 5, 4, 4, 4, 5, 5,4]))



def tri_comptage(l, m):
    l_p = [0 for _ in range(m + 1)]
    for i in l:
        l_p[i] += 1
    l_pp = []
    for i in range(len(l_p)):
        l_pp += [i]*l_p[i]
    return l_pp

#print(tri_comptage([9, 8, 7, 6, 5, 4, 4, 4, 5, 5,4], 9))


def convertir_liste(n , m):
    if m == 0 and n != 0:
        return "Impossible to do it with {} digits".format(m)
    elif m == 0:
        return []
    else:
        return convertir_liste(n//10, m - 1) + [n % 10]


def convert_num(l):
    s = 0
    for i in l:
        s = s * 10 + i
    return s


#print(convert_num(convertir_liste(823, 3)))

def tri_radix(l, m):
    l = [convertir_liste(k, m) for k in l]
    for k in reversed(range(m)):
        for i in range(1, len(l)):
            j = i
            while l[j][k] < l[j - 1][k] and j > 0:
                l[j], l[j-1] = l[j - 1], l[j]
                j -= 1
    l = [convert_num(e) for e in l]
    return l

#print(tri_radix([764, 199, 20, 438, 199], 3))


def tri1parpas(l, d, p):
    for i in range(d, len(l), p):
        k = i
        while k > p - 1 and l[k - p] > l[k]:
            l[k - p], l[k] = l[k], l[k - p]
            k -= p
    return l


#print(tri1parpas([764, 199, 20, 438, 199], 0, 1))


def tripartas(l, p):
    for i in range(p, 2*p):
        l = tri1parpas(l, i, p)
    return l

#print(tripartas([9, 8, 7, 6, 5, 4, 4, 4, 5, 5,4], 1))

def trishell(l):
    l_p = [1]
    while l_p[0]*3 + 1 < len(l):
        l_p = [l[0]*3 + 1] + l_p
    for p in l_p:
        l = tripartas(l, p)
    return l

#print(trishell([9, 8, 7, 6, 5, 4, 4, 4, 5, 5,4]))


def tri(l):
    for i in range(1, len(l)):
        k = i
        while (l[k].couleur() == 'rouge' and l[k - 1].couleur != 'rouge') or (l[k].couleur == 'blanc' and l[k - 1].couleur == 'bleu'):
            l[k], l[k - 1] = l[k - 1], l[k]
            k -= 1
    return l