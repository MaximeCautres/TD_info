width = int(input())
height = int(input())
n = int(input())
positions = []
composition = []
move = [[0, 1], [1, 0], [0, -1], [-1, 0]]
maps = [[-1 for _ in range(height + 2)] for _ in range(width + 2)]


def parcours_contour(d, c, i, l):
    if c == d:
        return [k for k in range(n) if k in l]
    else:
        l.append(maps[c[0]][c[1]])
        j = i - 1
        while maps[c[0]+move[j % 4][0]][c[1]+move[j % 4][1]] == -1:
            j += 1
        return parcours_contour(d, [c[0]+move[j % 4][0], c[1]+move[j % 4][1]], j % 4, l)



for i in range(n):
    positions.append([i + 1 for i in list(map(int, input().split()))])
    for x in range(positions[-1][0], positions[-1][2]):
        for y in range(positions[-1][1], positions[-1][3]):
            maps[x][y] = i

for i in maps:
    print(i)

for i in range(n):
    s = True
    for friends in composition:
        if i in friends:
            s = False
    if s:
        x = positions[i][0]
        y = positions[i][1]
        j = 0
        s = True
        while (maps[x + move[j % 4][0]][y + move[j % 4][1]] != -1 or
               maps[x + move[(j+1) % 4][0]][y + move[(j+1) % 4][1]] == -1) and j < 4:
            j += 1
            s = s and (maps[x + move[j%4][0]][y  + move[j%4][1]] != -1)
        if j == 4 and not s:
            composition.append([i])
        if j <= 3:
            composition.append(parcours_contour([x, y], [x + move[(j+1) % 4][0],
                                                         y + move[(j+1) % 4][1]], j + 1, [maps[x][y]]))

print(composition)

