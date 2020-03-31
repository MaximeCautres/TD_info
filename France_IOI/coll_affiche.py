N = int(input())

l = []
# l[][taille, a qui il renvoie, combien de visible]

for i in range(N):
    entry = list(input().split())

    maxi = 0
    
    if entry[0] == "C":

        current = int(entry[1])

        k = i - 1

        if current >= maxi:
            l.append([current, -1, 1])
            maxi = current
        else:
            while k != -1:
                if l[k][0] > current:
                    l.append([current, k, l[k][2]+1])


    else:
        
