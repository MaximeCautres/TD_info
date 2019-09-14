import random
from math import factorial

"""exercise 1"""


def ex1():
    s = 1
    i = 1
    while s < 8/5:
        i += 1
        s += (1/i**2)
    return i


"""Ici la complexite est fixe, mais si l'on s'interesse a un minimum precis que l'on nomme m:
pour n grand, on a H(n)~ln(n) donc ici on a n >= e^(n)
D'ou une complexite exponentielle
"""

"""exercise 2"""

"""1) Lorsque l'on fait repete ce procede, on obtient les chiffre qui compose le nombre du poid faible au poid fort"""

"""2 et 3"""


def cubsum(n):
    if n < 10:
        return n
    else:
        return cubsum(n//10) + (n % 10)**3


"""la complexite de cette fonction est en log_10(n) appels recursifs avec 3 multiplications par appel"""

""" 4 """


def somme_cube(n):
    n = str(n)
    s = 0
    for i in n:
        s += int(i)**3
    return s


"""la complexite est similaire a celle du programme recursif"""


def fixpoint(f, n):
    return [i for i in range(0, n+1) if i == f(i)]


"""complexite en n"""

"""Exercise 3"""


def randintlist(n, p):
    return [random.randrange(1, p+1) for _ in range(n)]


"""complexite en n et depandat de celle en p de la fonction randrange"""

"""exercise 4"""


def mean(l):
    s = 0
    for i in l:
        s += i
    return s/len(l)


def ecart_type(l):
    return pow(mean(list(map(lambda x: x**2, l)))-mean(l)**2, 0.5)


"""exercise 5"""


def maxi(l):
    m = l[0]
    for i in l[1:]:
        if i > m:
            m = i
    return m


def maxi_indice(l):
    m = l[0]
    l_p = []
    for i in l[1:]:
        if i >= m:
            m = i
    for i in range(len(l)):
        if l[i] == m:
            l_p += [i]
    return m, l_p


def two_max(l):
    m = maxi(l)
    l_p = []
    for i in l:
        if i != m:
            l_p += [i]
    return m, maxi(l_p)


def without_two_max(l):
    m, m_p = two_max(l)
    l_p = []
    for i in l:
        if i != m and i != m_p:
            l_p += [i]
    return l_p


"""exercise 6"""


def rand_sentence(n, p):
    s = ''
    for _ in range(n):
        i = random.randrange(0, 26+int(26*p/(1-p)))
        if i >= 26:
            s += ' '
        else:
            s += chr(ord('a')+i)
    return s


def a_z(p):
    s = ''
    for i in p:
        if i == 'a':
            s += 'z'
        else:
            s += i
    return s


def max_appearance(p):
    l = [0 for _ in range(26)]
    for i in p:
        if i != ' ':
            if i.isupper():
                l[ord(i)-ord('A')] += 1
            elif i.islower():
                l[ord(i)-ord('a')] += 1
    return list(map(lambda x: chr(x+ord('a')), maxi_indice(l)[1])), maxi(l)


"""exercise on Permutations"""


def check(p):
    l = [False for _ in range(len(p)+1)]
    for i in p:
        l[i] = True
    return [True]*len(p) == l[1:]


def disjoint_sup_dec(p):
    p = [0] + p    # on s'occupe de decaller les indices
    l_sup = []    # liste des cycles
    for i in range(1, len(p)):
        if p[i] != 0:   # on choisi que des valeurs qui commence un nouveau cycle
            l = [p[i]]  # on initialise le cycle avec cette valeur
            p[i] = 0    # on la supprime de la liste car elle a deja servie
            while p[l[-1]] != 0:   # tant que la case sur laquelle on va n'a pas deja servi
                l += [p[l[-1]]]    # on l'ajoute au cycle
                p[l[-2]] = 0       # on la supprime des cases disponibles
            if len(l) > 1:    # on s'assure que le cycle permutte au moins deux elements
                l_sup += [l]        # quand le cycle en fini, on l'ajoute a la liste des cycles
    return l_sup


""" 3 """


def S_shift_base_k(l):  # permettre une incrementation facile selon l'ordre lexicographique
    l_p = [i for i in l]
    try:                # il est possible que le successeur necessite une liste plus longue, d'ou le risque d'erreur
        l_p[-1] += 1
        for i in range(len(l)):
            if l_p[-i-1] > len(l):
                l_p[-i-2] += l_p[-1-i]-len(l)
                l_p[-1-i] = 1
        return l_p
    except:   # en cas de liste plus longue, on sait qu'il ne s'agit pas d'une permutation
                # donc on lui associe la viste vide qui est vrai pour les permuttation et
                # qui est facilement identifiable
        return []


def pro(p):
    while not check(S_shift_base_k(p)):     # on test tout les combinaison dans l'ordre
                                            # lexicographique jusqu'a en trouve une qui est une permutation
        p = S_shift_base_k(p)
    p = S_shift_base_k(p)
    if p == []:
        return False                        # on sort le cas ou elle n'existe pas
    else:
        return p                            # sinon on l'ajoute


""" Q4"""


def all_perm(n):
    l = [[i for i in range(1, n+1)]]
    for _ in range(factorial(n)-1):
        l += [pro(l[-1])]
    return l

# print(all_perm(4))


""" Q5 """


def rang(p):
    s = factorial(len(p))
    while pro(p) != False:
        p = pro(p)
        s -= 1
    return s

print(rang([1,2,4,3,5]))







