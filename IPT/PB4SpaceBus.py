
def space_bus(n, p, t, m):
    """
    :param n: le nombre de stations à visiter
    :type n: int
    :param p: le nombre de personnes à prendre dans chaque station
    :type p: list[int]
    :param t: le temps de prise en charge de chaque station
    :type t: list[int]
    :param m: le temps dont joseph dispose avant le début du concours
    :type m: int
    """
    # TODO Afficher le nombre maximal de personnes que Joseph pourra amener au
    # concours

    def Knapsack(value, weight, max_weight):

        global table
        
        table = [[None for _ in range(max_weight + 1)] for _ in range(len(value))]

        #initialisation

        for i in range(max_weight + 1):
            if i >= weight[0]:
                table[0][i] = value[0]
            else:
                table[0][i] = 0

        def aux(p, o):
            global table
            if table[o][p] == None:
                if p >= weight[o]:
                    aux(p, o-1)
                    aux(p - weight[o], o - 1)
                    table[o][p] = max(table[o-1][p], table[o-1][p- weight[o]] + value[o])
                    return ()
                else:
                    aux(p, o - 1)
                    table[o][p] = table[o-1][p]
                    return ()
            else:
                return ()

        aux(max_weight, len(value) - 1)

        print(table[len(value) - 1][max_weight])
        return ()   

    Knapsack(p, t, m)
    
if __name__ == '__main__':
    n = int(input())
    p = list(map(int, input().split()))
    t = list(map(int, input().split()))
    m = int(input())
    space_bus(n, p, t, m)
