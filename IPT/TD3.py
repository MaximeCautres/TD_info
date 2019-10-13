# -*- coding: utf-8 -*-
"""
@author: chris
"""


class Pile(object):
    def __init__(self):
        self.pile = []

    def is_empty(self):
        return self.pile == []

    def push(self, x):
        self.pile.append(x)

    def pop(self):
        if self.is_empty():
            raise ValueError("pile vide")
        return self.pile.pop()


def peek(p):
    if p.is_empty():
        return
    else:
        x = p.pop()
        p.push(x)
    return x


def taille(p):
    q = Pile()
    n = 0
    while not p.is_empty():
        q.push(p.pop())
        n += 1
    while not q.is_empty():
        p.push(q.pop())
    return n


def copie(p):
    q = Pile()
    d = Pile()
    while not p.is_empty():
        q.push(p.pop())
    while not q.is_empty():
        p.push(peek(q))
        d.push(q.pop())
    return d


def inversePile(p):
    p_p = copie(p)
    q = Pile()
    while not p_p.is_empty():
        q.push(p_p.pop())
    return q


def swap2(p):
    c = p.pop()
    d = p.pop()
    p.push(c)
    p.push(d)
    return p


def ieme(p, i):
    q = Pile()
    for _ in range(i - 1):
        q.push(p.pop())
    x = peek(p)
    while not q.is_empty():
        p.push(q.pop())
    return x


"""exercice 10 TD2 """


def bienpar(e):
    p = Pile()
    op = "(["
    cl = ")]"
    S = True
    for i in e:
        if i in op or i in cl:
            if i in op:
                p.push(i)
            elif peek(p) == op[cl.index(i)]:
                p.pop()
            else:
                S = False
    return p.is_empty() and S


"""exercice 11 TD2 """


def evalue(e):
    p = Pile()
    e = e.split(' ')
    operateur = "+-*/"
    for i in e:
        if i in operateur:
            x = [0, 0, 1, 1]
            x[operateur.index(i)] = p.pop()
            p.push(((((p.pop()+x[0])-x[1])*x[2])/x[3]))
        else:
            p.push(int(i))
    return p.pop(), p.is_empty()


p = Pile()
p.push(1)
p.push(2)
p.push(3)
p.push(4)

# print(p.pile)
# print(peek(p))
# print(taille(p))
# q = copie(p)
# print(p.pile)
# print(q.pile)
# print(inversePile(q).pile)
# print(swap2(p).pile)
# print(p.pile)
# print(ieme(p, 2))

#print(bienpar("(([()[]()]))"))

print(evalue("1 2 * 3 4 * + 2 *"))

