n = int(input())
xn = []
for k in range(0, 2**n - 1):
    for i in range(1, n+1):
        if (k + 1 - 2**(i-1))%(2**i) == 0:
            q = (k + 1 - 2**(i-1))//(2**i)
            if q % 2 == 0:
                xn.append(0)
            else:
                xn.append(1)
print(xn)
