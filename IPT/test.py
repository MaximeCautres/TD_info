def tarjan(graphe):
        global state, low_link, k, pile, composantes, n
        n = 11
        composantes = []
        state = [-1 for _ in range(n)]
        low_link = [-1 for _ in range(n)]
        k = 0
        pile = []

        def aux(s_p):
            global state, low_link, k, pile, composantes, n
            print()
            if state[s_p] == -1:
                pile.append(s_p)
                state[s_p] = k
                low_link[s_p] = k
                k += 1
                for voisin in graphe[s_p]:
                    it_low_link = aux(voisin)
                    if it_low_link != -1:
                        low_link[s_p] = min(it_low_link, low_link[s_p])
                print("fin de traitement de ", s_p, " de lowlink", low_link[s_p])
                print("state:", state)
                print("low_link:", state)
                print("composantes:", composantes)
                # maintenant que l'on à la low_link, il faut voir si on est sur le début d'une composante
                if low_link[s_p] == state[s_p]:
                    ind = list(map(lambda x : state[x], pile)).index(state[s_p])
                    pile, compo = pile[:ind], pile[ind:] 
                    composantes.append(compo)
                    for i in compo:
                        state[i] = -2
                    return -1
                else:
                    return low_link[s_p]
            else:
                if state[s_p] == -2:
                    return -1
                else:
                    return state[s_p]

        for i in range(n):
            if state[i] == -1:
                _ = aux(i)
        print(composantes)
        return

def tarjan_bis(graphe):
        global composantes, pile, en_attente, k, state
        composantes = []
        pile = []
        en_attente = [False] * len(graph)
        k = 0
        state = [None]*len(graph)

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
        return composantes
                

graph = [[1], [2], [3,6], [0, 5], [5], [4], [7], [9], [7], [0, 10], [4, 6]]
print(tarjan_bis(graph))
