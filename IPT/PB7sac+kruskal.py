import sys
import os.path

sys.setrecursionlimit(100000)

def printf(x):
    if os.path.isfile('print.txt'):
        print(x)

def radars_imperiaux(n, m, routes, c, contrats):
    """
    :param n: le nombre de planètes à relier
    :type n: int
    :param m: le nombre de routes possibles
    :type m: int
    :param routes: la liste des routes possibles
    :type routes: list[dict["a": int, "b": int, "l": int]]
    :param c: le nombre de contrats
    :type c: int
    :param contrats: la liste des contrats possibles
    :type contrats: list[dict["d": int, "r": int]]
    """
    # TODO Affichez la longueur  minimale de route à construire pour relier
    # toutes les planètes, puis sur la ligne suivant le nombre maximal de
    # radars pouvant être installé sur ces routes.

    def fusion(l1, l2):
        l = []
        for _ in range(len(l1) + len(l2)):
            if l1 == []:
                return l + l2
            elif l2 == []:
                return l + l1
            elif l1[0]["l"] < l2[0]["l"]:
                l += [l1[0]]
                l1 = l1[1:]
            else:
                l += [l2[0]]
                l2 = l2[1:]

    def tri_fusion(l):
        if len(l) <= 1:
            return l
        else:
            return fusion(tri_fusion(l[:len(l)//2]), tri_fusion(l[len(l)//2:]))
        
    def creer_classe(l):
        for i in range(len(l)):
            l[i]["u"]=i
        return ()

    def find(l, x):
        if l[x]["u"] != x:
            l[x]["u"] = find(l, l[x]["u"])
        return l[x]["u"]

    def union(l, x, y):
        rx, ry = find(l, x), find(l, y)
        if rx < ry:
            l[ry]["u"] = rx
        elif ry < rx:
            l[rx]["u"] = ry
        return ()

    def kruskal(routes):
        arretes = []
        sommets = [dict() for _ in range(n)]
        sorted_arrete = tri_fusion(routes)
        creer_classe(sommets)
        printf(("arretes triees:", sorted_arrete))
        for arrete in sorted_arrete:
            printf(("sommet:", sommets))
            if find(sommets, arrete["a"]-1) != find(sommets, arrete["b"]-1):
                arretes.append(arrete)
                union(sommets, arrete["a"]-1, arrete["b"]-1)
        return arretes

    
    graphe_final = kruskal(routes)
    printf(("graphe final:", graphe_final))

    longueur = 0

    for arrete in graphe_final:
        longueur += arrete["l"]

    print(longueur, end = " ")

    def sac_a_dos(poids_max, liste_objet):
        global tableaux
        tableaux = dict()

        printf(tableaux)

        # calcul du résultat par memoisation

        def aux(objet, poid, liste_objet):
            global tableaux
            if not str(objet) + ";" + str(poid) in tableaux:
                if objet == 0:
                    tableaux[str(objet) + ";" + str(poid)] = liste_objet[objet]["r"] * (poid >= liste_objet[objet]["d"])
                    return ()
                elif poid >= liste_objet[objet]["d"]:
                    aux(objet-1, poid, liste_objet)
                    aux(objet-1, poid - liste_objet[objet]["d"], liste_objet)
                    tableaux[str(objet) + ";" + str(poid)] = max(tableaux[str(objet-1) + ";" + str(poid)], tableaux[str(objet-1) + ";" + str(poid - liste_objet[objet]["d"])] + liste_objet[objet]["r"])
                    printf(tableaux)
                    return ()
                else:
                    aux(objet-1, poid, liste_objet)
                    tableaux[str(objet) + ";" + str(poid)] = tableaux[str(objet-1) + ";" + str(poid)]
                    printf(tableaux)
                    return ()
            else:
                return ()

        aux(len(liste_objet) - 1, poids_max, liste_objet)

        return tableaux[str(len(liste_objet) - 1) + ";" + str(poids_max)]

    nbr_radar = sac_a_dos(longueur, contrats)
    print(nbr_radar)
    
if __name__ == '__main__':
    n = int(input())
    m = int(input())
    routes = [dict(zip(("a", "b", "l"), map(int, input().split()))) for _ in range(m)]
    c = int(input())
    contrats = [dict(zip(("d", "r"), map(int, input().split()))) for _ in range(c)]
    radars_imperiaux(n, m, routes, c, contrats)
