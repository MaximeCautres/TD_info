n = int(input())
for j in range(1,n+1):
    xn = []
    for k in range(0, 2**j - 1):
        for i in range(1, j+1):
            if (k + 1 - 2**(i-1))%(2**i) == 0:
                q = (k + 1 - 2**(i-1))//(2**i)
                if q % 2 == 0:
                    xn.append(0)
                else:
                    xn.append(1)
    print(xn)
