xn = [0]
n = int(input())
k = int(input())
for loop in range(n):
    xnn = [0]
    for i in range(0,len(xn)-1,2):
        xnn = xnn + [xn[i]] + [1] + [xn[i+1]] + [0]
    xnn = xnn + [xn[-1]] + [1]
    xn = xnn
print (xn[k])
