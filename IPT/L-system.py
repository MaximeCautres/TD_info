from turtle import *
from math import sqrt

def dessiner(u, a, motif):
    for i in motif:
        if i == "F":
            forward(u)
        elif i == "+":
            left(a)
        elif i == "-":
            right(a)
        else:
            return 1
    return 0

#dessiner(100, 120, "F+F+F")

def suivant(motif, regle):
    nw = ""
    for i in motif:
        if i == "F":
            nw += regle
        else:
            nw += i
    return nw

#print(suivant("F--F--F", "F+F--F+F"))

def evolution(axiome, regle, etape):
    w = axiome
    for _ in range(etape):
        w = suivant(w, regle)
    return w

#print(evolution("F", "F+", 4))

from  matplotlib.pyplot import *
"""
X = [0,0,2,4,4]
Y=[0,4,2,4,0]
plot(X,Y,'k-')
show()"""

def triangle_plt(unit):
    X = [0, unit, unit/2, 0]
    Y = [0, 0, sqrt(3)/2*unit, 0]
    plot(X,Y,'k-')
    show()

#triangle_plt(300)
            
def new_pos(x, y, d, c, l, a):
    if c == "+":
        d += a
    elif c == "-":
        d -= a
    elif c == "F":
        x += l * cos(radians(d))
        y += l * sin(radians(d))
    return x, y, d

def dessine_plt(l, a, motif):
    X = [0]
    Y = [0]
    d = 0
    for i in motif:
        x, y, d = new_pos(X[-1], )
        
