import numpy as np

count = 0
number = 1000000

for k in range(number + 1):
    m = np.random.randint(2, size=(20, 20))
    det = round(np.linalg.det(m))
    if  det % 2 == 1:
        count += 1
    if k % 100 == 0 and k != 0:
        print(count/k)

print(count/number)
