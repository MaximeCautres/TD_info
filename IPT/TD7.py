import numpy as np
import matplotlib.pyplot as plt
import math

def q1():
    x = np.linspace(0, 1, 1000)
    y = [math.cos(i) for i in x]
    plt.plot(x, y)
    plt.show()
    return 
#q1()

def q2():
    x = np.linspace(-5, 2, 1000)
    def f(x):
        if x <= 0:
            return math.cos(x)
        else:
            return math.exp(x)
    y = [f(i) for i in x]
    plt.plot(x, y)
    plt.show()
    return

#q2()

def q3():
    t = np.linspace(0, 2*math.pi, 1000) 
    x = []
    y = []
    def f_p(t):
        return (1/math.sqrt(3))*math.cos(t), math.sin(t)
    for t_i in t:
        x_i, y_i = f_p(t_i)
        x.append(x_i)
        y.append(y_i)
    plt.plot(np.array(x), np.array(y))
    plt.show()
    return

#q3()

def q4():
    def f(x):
        return math.sin(x)/(1+x**3)
    x = np.linspace(0, math.pi, 1000)
    s = 0
    for i in x[:-1]:
        s += f(i)
    print( s * (x[1] - x[0]))
    return

#q4()
    
def q5():
    def f(x):
        return math.sin(x)/(1+x**3)
    x = np.linspace(0, math.pi, 1000)
    s = 0
    for i in range(x.shape[0] - 1):
        s += (f(x[i+1])+f(x[i])) / 2
    print( s * (x[1] - x[0]))
    return

#q5()

def q5_p():
    def f(t):
        return (1/math.sqrt(3))*math.cos(t), math.sin(t)
    t = np.linspace(0, 2 * math.pi, 1000)
    s = 0
    for i in range(t.shape[0] - 1):
        s += ((f(t[i])[1]+f(t[i + 1])[1]) / 2 ) * (f(t[i])[0]-f(t[i+1])[0])
    print(s)
    return

#q5_p()

def q6():
    x_inf, x_sup = -100, 100
    k = 0
    def f(x):
        return x ** 3 + x + 2
    while x_sup - x_inf > 10**-12:
        k += 1
        if f((x_inf + x_sup)/2) <= 0:
            x_inf = (x_inf + x_sup)/2
        else:
            x_sup = (x_inf + x_sup)/2
    print( (x_inf + x_sup)/2, k)
    return

#q6()

def q7():
    epsilon = 10**-2
    h = 10 ** -2
    def f(x):
        return x ** 3 + x + 2
    a = [1000, 1000 + epsilon * 2]
    k = 0
    while abs(a[0] - a[1]) > epsilon:
        pente = (f(a[k%2]+h) - f(a[k%2]-h))/(2*h)
        k += 1
        a[k % 2] = a[(k + 1)%2] - f(a[(k+1)%2])/pente
    print(a[k%2])
    return

#q7()

def F1(y, t=None):
    return -2*y


def F2(y, t=None):
    return 1 + y**2


def q9(F, y0, a, b, n):
    p = (b - a) / n
    x = [a + p * k for k in range(n + 1)]
    y = [y0]
    for _ in range(n):
        y.append(y[-1] + p*F(y[-1]))
    plt.plot(x, y)
    plt.show()
    return

#q9(F2, 0, 0, 1.5, 10000)

def F3(y, t=None):
    return np.array([y[1], -y[0]-y[1]])

def q9_bis(F, y0, a, b, n): # version plus gros ordre ou plus d'equation
    p = (b-a) / n
    x = [a + p * k for k in range(n + 1)]
    y = [np.array(y0)]
    for _ in range(n):
        y.append(y[-1] + p*F(y[-1]))
    plt.plot(x, y)
    plt.show()

q9_bis(F3, [1, 1], 0, 1, 1000)



