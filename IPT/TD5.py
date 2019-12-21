import matplotlib.pyplot as plt
import numpy as np
import random


def red(im):
    x, y, z = im.shape
    new_im = np.zeros((x, y, z))
    new_im[:, :, 0] = im[:, :, 0]
    return new_im


def green(im):
    x, y, z = im.shape
    new_im = np.zeros((x, y, z))
    new_im[:, :, 1] = im[:, :, 1]
    return new_im


def blue(im):
    x, y, z = im.shape
    new_im = np.zeros((x, y, z))
    new_im[:, :, 2] = im[:, :, 2]
    return new_im


def rot90(im):
    x, y, z = im.shape
    new_image = np.zeros((y, x, z))
    for ny in reversed(range(x)):
        new_image[:, ny] = im[ny]
    return new_image


def sym(im):
    x, y, z = im.shape
    new_image = np.zeros((x, y, z))
    for ny in range(y):
        new_image[:, y - 1 - ny] = im[:, ny]
    return new_image


def cross(im):
    x, y, z = im.shape
    new_image = np.copy(im)
    coeff = y / x
    for nx in range(x):
        y1, y2 = int(coeff * nx), int(y - 1 - coeff * nx)
        new_image[nx, y1, 0] = 255
        new_image[nx, y2, 0] = 255
    return new_image


def switch(im, n):
    x, y, z = im.shape
    new_image = np.copy(im)
    x1, y1, x2, y2 = random.randrange(0, x - n), random.randrange(0, y - n), \
                     random.randrange(0, x - n), random.randrange(0, y - n)
    new_image[x1: x1 + n, y1: y1 + n], new_image[x2: x2 + n, y2: y2 + n] = \
        im[x2: x2 + n, y2: y2 + n], im[x1: x1 + n, y1: y1 + n]
    return new_image


def flews_gaussians(im):
    x, y, z = im.shape
    new_image = np.copy(im)
    for nx in range(1, x - 1):
        for ny in range(1, y - 1):
            new_image[nx, ny] = 1 / 9 * np.sum(im[nx - 1: nx + 2, ny - 1: ny + 2], axis=(0, 1))
    return new_image


def alter(im1, im2):
    x, y, z = im1.shape
    new_image = np.zeros((2 * x, y, z))
    for nx in range(x):
        new_image[2 * nx] = im1[nx]
        new_image[2 * nx + 1] = im2[nx]
    return new_image


def fusion(im1, im2):
    return (im1 + im2) / 2


def photomaton(im):
    x, y, z = im.shape
    new_image = np.zeros((x, y, z))
    for i in range(x // 2):
        for j in range(y // 2):
            new_image[i, j] = im[2 * i, 2 * j]
            new_image[i, y // 2 + j] = im[2 * i, 2 * j + 1]
            new_image[x // 2 + i, j] = im[2 * i + 1, 2 * j]
            new_image[x // 2 + i, y // 2 + j] = im[2 * i + 1, 2 * j + 1]
    return new_image


def rot(im, teta):
    from math import sqrt
    from math import cos, sin
    x, y, z = im.shape
    r = np.array([[cos(-teta), -sin(-teta)], [sin(-teta), cos(-teta)]])
    new_image = np.zeros((int(sqrt(x ** 2 + y ** 2)), int(sqrt(x ** 2 + y ** 2)), 3))
    ncenter = (int(sqrt(x ** 2 + y ** 2)) // 2, sqrt((x ** 2 + y ** 2)) // 2)
    center = (x // 2, y // 2)
    x_min, x_max, y_min, y_max = x, 0, y, 0
    for nx in range(int(sqrt(x ** 2 + y ** 2))):
        for ny in range(int(sqrt(x ** 2 + y ** 2))):
            [c1, c2] = np.dot(r, np.array([nx - ncenter[0], ny - ncenter[1]]))
            if 0 <= int(c1) + center[0] < x and 0 <= int(c2) + center[1] < y:
                new_image[nx, ny] = im[int(c1) + center[0], int(c2) + center[1]]
                if x_min >= nx :
                    x_min = nx
                if y_min >= ny:
                    y_min = ny
                if x_max <= nx:
                    x_max = nx
                if y_max <= ny:
                    y_max = ny

    return new_image[x_min:x_max+1, y_min:y_max+1]

from math import *

image1 = plt.imread("blaise.png")
image2 = plt.imread('lena.png')
print(image2.shape)
plt.imshow(rot(green(image2), -pi/4))
plt.show()
