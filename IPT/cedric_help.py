"""Solution 1 naive"""

def somme_poids(list_poid, l):
    s = 0
    for i in l:
        s += list_poid[i]
    return s

def somme_valeur(list_valeur, l):
    s = 0
    for i in l:
        s += list_valeur[i]
    return s

def partie(l, list_poid, poids_max):
    if l == []:
        return [[]]
    else:
        pred = partie(l[:-1], list_poid, poids_max)
        return pred + [p + [l[-1]] for p in pred if somme_poids(list_poid, p + [l[-1]]) <= poids_max]

def main(n, list_valeur, list_poid, poids_max):
    maxi = 0
    for p in partie([j for j in range(n)], list_poid, poids_max):
        if somme_valeur(list_valeur, p) > maxi:
            maxi = somme_valeur(list_valeur, p)
    return maxi


print(main(4, [5,3,3,2], [6,3,3,2], 6))

""" Solution deux, récursive et assez optimisé, cependant elle est améliorable en utilisant la mémoisation"""

def knapsac(list_poid, list_valeur, poids_max):
    l = [[0 for _ in range(poids_max+1)] for _ in range(len(list_valeur))]
    for i in range(poids_max+1):
        if i >= list_poid[0]:
            l[0][i]=list_valeur[0]
    for i in range(1, len(list_valeur)):
        for j in range(poids_max + 1):
            if j >= list_poid[i]:
                l[i][j] = max(l[i][j-list_poid[i]], l[i-1][j])
            else:
                l[i][j] = l[i-1][j]
    return l[-1][-1]


print(knapsac([5,3,3,2], [6,3,3,2], 6))

""" Solution trois, recursive avec memoisation"""

def knapsac_pro(list_poid, list_valeur, poids_max):
    global l
    l = [['a' for _ in range(poids_max+1)] for _ in range(len(list_valeur))]

    def aux(i, j):
        global l
        if i == 0:
            l[i][j] = list_valeur[0] if j >= list_poid[0] else 0
            return l[i][j]
        else:
            if l[i][j] == 'a':
                if j >= list_poid[i]:
                    return max(aux(i, j-list_poid[i]), aux(i-1, j))
                else:
                    return aux(i-1, j)
            else:
                return l[i][j]
    return aux(len(list_valeur) - 1, poids_max) 

                

print(knapsac_pro([5,3,3,2], [6,3,3,2], 6))
