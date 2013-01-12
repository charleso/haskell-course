f = open('pinball.txt')
main_list = []
for line in f:
    main_list.append(line.split())
    print(line.split())
    
cache = []
for i in range(len(main_list)):
    sublist = []
    for z in range(len(main_list)):
        sublist.append(0)
    cache.append(sublist)

def foobar(x,y):
    if x < 0:
        return 0
    values = []
    if y < 18:
        for i in range(-1,2):
            try:
                if cache[x+i][y+1] != 0:
                    values.append(cache[x+i][y+1])
                    continue
            
                values.append(foobar(x+i,y+1))
                cache[x+i][y+1] = values[-1]
            except IndexError:
                continue
    else:
        return int(main_list[y][x])
    if values:
        return max(values) + int(main_list[y][x])
    else: return int(main_list[y][x])



most_points = 0
for col in range(len(main_list)):
    ans = foobar(col,0)
    if ans > most_points:
        most_points = ans

print(most_points)
