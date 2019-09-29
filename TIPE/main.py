import random
from math import inf
import numpy as np
import matplotlib.pyplot as plt
import scipy.integrate as integrate
import matplotlib.animation as animation

height = 10
width = 10
dim = np.array([height, width])
start = np.array([1, 1])  # start position
finish = np.array([height-2, width-2])  # finish position
gamma = 1
current = start.copy()
moves = np.array([[1, 0], [-1, 0], [0, 1], [0, -1]])

grid = np.array([[False for _ in range(height)] for _ in range(width)])
grid[1:-1, 1:-1] = 0


def move_from():
    global current
    if random.random() <= gamma:
        next_pos = np.array([0, 0])
        while not grid[next_pos[0], next_pos[1]]:
            next_pos = current + np.random.choice(moves)
        current = next_pos
    else:
        best_value = -inf
        best = None
        for mv in moves:
            next_pos = current + mv
            value = grid[next_pos[0], next_pos[1]]
            if value and best_value < value:
                best_value = value
                best = next_pos
        current = best


def update_grid(previous):



def make_a_step():
    previous = current.copy()
    move_from()
    update_grid(previous)


make_a_step()
