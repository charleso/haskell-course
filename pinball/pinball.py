f = open('pinball.txt')
main_list = []
for line in f:
    main_list.append(map(int, line.split()))
    print(line.split())

cache = []
for i in range(len(main_list)):
    sublist = []
    for z in range(len(main_list)):
        sublist.append(0)
    cache.append(sublist)

max_length = len(main_list)

def foobar(x,y):
    if x < 0 or x == max_length or y == max_length:
        return 0
    if cache[x][y] != 0:
        return cache[x][y]
    values = []
    for i in range(-1,2):
        values.append(foobar(x+i,y+1))
    value = max(values) + main_list[y][x]
    cache[x][y] = value
    return value

most_points = 0
for col in range(len(main_list)):
    ans = foobar(col,0)
    if ans > most_points:
        most_points = ans

print(most_points)
#1036
#1246?