width = int(input())
height = int(input())
n = int(input())
positions = []
composition = [i for i in range(n)]
maps = [[[] for _ in range(height + 2)] for _ in range(width + 2)]
for i in range(n):
    positions.append([i + 1 for i in list(map(int, input().split()))])
    for x in range(positions[-1][0], positions[-1][2]):
        for y in range(positions[-1][1] ,positions[-1][3]):
            maps[x][y].append(i)
alone = [True]*n
for i in range(n):
    if alone[i]:
        checkneighbours(i,i)
compo = []
for i in range(n):
	if composition[i] != -1:
		compo.append(composition[i])

s += 1
for c in compo:
	if len(c) == 1:
		s += min(abs(positions[c[0]][0]-positions[c[0]][2], abs(positions[c[0]][1]-positions[c[0]][3])))
	else:
		

def checkneighbours(d, k):
    personnal_neighbours = []
    for x in range(positions[k][0] - 1, positions[k][2] + 1):
        if maps[x][positions[k][1] - 1] != -1:
            if maps[x][positions[k][1] - 1] not in composition[d]:
                composition[d].append(maps[x][positions[k][1] - 1])
                personnal_neighbours.append(maps[x][positions[k][1] - 1])
		if k!= d:
			composition[k] = -1
		alone[k] = False
    for x in range(positions[k][0] - 1, positions[k][2] + 1):
        if maps[x][positions[k][3] + 1] != -1:
            if maps[x][positions[k][3] + 1] not in composition[d]:
                composition[d].append(maps[x][positions[k][3] + 1])
                personnal_neighbours.append(maps[x][positions[k][3] + 1])
                if k != d:
			composition[k] = -1
		alone[k] = False
    for y in range(positions[k][1] - 1, positions[k][3] + 1):
        if maps[y][positions[k][0] - 1] != -1:
            if maps[y][positions[k][0] - 1] not in composition[d]:
                composition[d].append(maps[y][positions[k][0] - 1])
                personnal_neighbours.append(maps[y][positions[k][0] - 1])
                if k != d:
			composition[k] = -1
		alone[k] = False
    for y in range(positions[k][1] - 1, positions[k][3] + 1):
        if maps[y][positions[k][2] + 1] != -1:
            if maps[y][positions[k][2] + 1] not in composition[d]:
                composition[d].append(maps[y][positions[k][2] + 1])
                personnal_neighbours.append(maps[y][positions[k][2] + 1])
                if k != d:
			composition[k] = -1
		alone[k] = False
    if personnal_neighbours:
        for i in personnal_neighbours:
            checkneighbours(d, i)
    return ()
