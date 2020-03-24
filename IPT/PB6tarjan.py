import sys

sys.setrecursionlimit(100000)


def bataille_par_elimination(n, s, m, chemins):
    """
    :param n: le nombre de troupes de soldats de l'empire
    :type n: int
    :param s: le nombre de soldats par troupes
    :type s: list[int]
    :param m: le nombre de chemins (dirigés) possibles d'une troupe à une autre
    :type m: int
    :param chemins: la liste des chemins possibles
    :type chemins: list[dict["a": int, "b": int]]
    """
    # TODO Affichez le nombre minimal de rebelles pour vaincre toutes les
    # batailles.

    global composantes
    
    adj = [[] for _ in range(n)]
    composantes = []
    
    # On cree la carte

    for c in chemins:
        adj[c[0]-1].append(c[1]-1)

    # definir tarjan

    def tarjan(graphe):
        global composantes, pile, en_attente, k, state
        pile = []
        en_attente = [False] * n
        k = 0
        state = [None]*n

        def dfs(s):
                global composantes, pile, en_attente, k, state
                pile.append(s)
                en_attente[s] = True
                state[s] = k
                k += 1
                low_link = k - 1
                for voisin in graphe[s]:
                        if state[voisin] == None:
                                low_link = min(low_link, dfs(voisin))
                        elif en_attente[voisin] and low_link > state[voisin]:
                                low_link = state[voisin]
                if low_link == state[s]:
                        composantes.append([])
                        while True:
                                u = pile.pop()
                                en_attente[u] = False
                                composantes[-1].append(u)
                                if u == s:
                                        break
                return low_link

        for s in range(len(graphe)):
                if state[s] == None:
                        dfs(s)
        return ()
    
    tarjan(adj)
    
    maximum_soldat = 0
    for compo in composantes:
        summ = 0
        for i in compo:
            summ += s[i]
        if summ > maximum_soldat:
            maximum_soldat = summ
    if maximum_soldat == 0:
        print(0)
    else:
        print(maximum_soldat + 1)
    return ()


if __name__ == '__main__':
    n = int(input())
    s = list(map(int, input().split()))
    m = int(input())
    chemins = [list(map(int, input().split())) for _ in range(m)]
    bataille_par_elimination(n, s, m, chemins)
