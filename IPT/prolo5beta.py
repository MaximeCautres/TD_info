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
        possible_ship_left = [positions[k].append(k) for k in range(n) if (-1)**(i//2)*positions[k][(i + 2) % 4] > (-1)**(i//2)*c[i % 2]
                         and (-1)**(i//2)*positions[k][i] <= (-1)**(i//2)*(c[i % 2] + (-1)**(i // 2))]
        possible_ship_straight = [positions[k].append(k) for k in range(n) if
                              (-1) ** (i // 2) * positions[k][(i + 2) % 4] > (-1) ** (i // 2) * c[i % 2]
                              and (-1) ** (i // 2) * positions[k][i] <= (-1) ** (i // 2) * (
                                          c[i % 2] + (-1) ** (i // 2))]
        State = i
        k = 0
        while State != (i + 1)%4:
            if c[(i+1)%2] <= possible_ship[k][(i+1)%2 + 2] and c[(i+1)%2] >= possible_ship[k][(i+1)%2]:
                State = (i + 1)%4
                if k not in l:
                    l.append(k)
            k += 1
        k =  0
        while State != (i - 1)%4:
            if c[(i+1)%2] <= possible_ship[k][(i+1)%2 + 2] and c[(i+1)%2] >= possible_ship[k][(i+1)%2]:
                State = (i - 1)%4
                if k not in l:
                    l.append(k)
            k += 1
        while State == i or c == d:
            c[0] += move[i][0]
            c[1] += move[i][1]
            # while State:
            #     if c[(i + 1) % 2] <= possible_ship[k][(i + 1) % 2 + 2] and c[(i + 1) % 2] >= possible_ship[k][
            #         (i + 1) % 2]:
            #         State = i + 1
            #         if k not in l:
            #             l.append(k)
            #     k += 1
            # while State:
            #     if c[(i + 1) % 2] <= possible_ship[k][(i + 1) % 2 + 2] and c[(i + 1) % 2] >= possible_ship[k][
            #         (i + 1) % 2]:
            #         State = i - 1
            #         if k not in l:
            #             l.append(k)

        return parcours_contour(d, c, i, l)




for i in range(n):
    positions.append([i + 1 for i in list(map(int, input().split()))])

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

# s = 0
# for i in composition:
#     if len(i) == 1:
#         s += min(positions[i[0]][2] - positions[i[0]][0], positions[i[0]][3] - positions[i[0]][1])
#     else:
#         cadre = i[0]
#         for j in i[1:]:
#             if j[0] < cadre[0]:
#                 cadre[0] = j[0]
#             if j[1] < cadre[1]:
#                 cadre[1] = j[1]
#             if j[2] > cadre[2]:
#                 cadre[2] = j[2]
#             if j[3] > cadre[3]:
#                 cadre[3] = j[3]
#

