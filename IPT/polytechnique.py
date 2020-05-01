def creerlistevide(n):
    return [0 for i in range(n+1)]

def estdansliste(l, x):
    for i in range(1, l[0] + 1):
        if i == x :
            return True
    return False

def ajoutedansliste(l, x):
    if estdansliste(l, x) and l[0] < len(l) - 1:
        l[0] += 1
        l[l[0]] = x
    return ()

def creerplansansroute(n):
    table = [[] for i in range(n+1)]
    table[0] = [n, 0]
    for i in range(1, n+1):
        table[i] = [0] * n
    return table

def est_route(plan, x, y):
    return estdansliste(plan[x], y)

def ajoute_route(plan, x, y):
    if not est_route(plan, x, y):
        plan[0][1] += 1
        plan[x][0] += 1
        plan[x][plan[x][0]] = y
        plan[y][0] += 1
        plan[y][plan[x][0]] = x
    return ()

def afficheTouteLesRoutes(plan):
    print("Ce plan contient "+ str( plan[0][1]) +" route(s):", end = " ")
    for i in range(1, plan[0][0]+1):
        for j in range(1, plan[i][0]):
            if i < plan[i][j]:
                print("("+str(i)+"-"+str(plan[i][j])+")", end = " ")
    return ()

from random import *

def coloriagealeatoire(plan, couleur, k, s, t):
    for i in range(plan[0][0]):
        if i == s:
            couleur[i+1] = 0
        elif i == t:
            couleur[i+1] = k + 1
        else:
            couleur[i+1] = randrange(1, k+1)
    return ()

def voisinedecouleur(plan, couleur, liste, c):
    l = creerlistevide(n)
    for ville in liste:
        for i in range(1, plan[i][0]):
            if couleur[ville] == c and estdansliste(l, plan[ville][i]):
                ajoute(l, x)
    return l

def existecheminarcenciel(plan, couleur, k, s, t):

    global state

    state = False
    
    def dfs(som, k_p):

        global state

        if som == t :
            state = True

        if k_p < k:
            for i in range(1, plan[som][0] + 1):
                if couleur[plan[som][i]] == k_p + 1:
                    dfs(plan[som][i], k_p + 1)
        return()

    dfs(s, 0)

    return state

def attendue(plan, couleur, k, s, t):
    sommet = [s]
    for k in range(1, k+2):
        sommet = voisinedecouleur(plan, couleur, sommet, k)
    return not sommet == []

def existecheminsimple(plan, k, s, t):
    couleur = creerlistevide(k+1)
    coloriageAleatoire(plan, couleur, k, s, t)
    return attendue(plan ,couleur, k ,s, t)

def attenduechemin(plan, couleur, k, s, t):
    sommet = [s]
    chemin = [[s]]
    for k in range(1, k+2):
        sommet = voisinedecouleur(plan, couleur, sommet, k)
        chemin.append(sommet)

    if sommet != []:
        true_chemin = [t]
        for k_p in reverse(range(0, k+1)):
            state = True
            for ville in chemin[k_p]:
                if state and estdansliste(plan[true_chemin[-1]], ville):
                    true_chemin += [ville]
                    state = False
        return true_chemin
    else:
        return False
                    
        
    
