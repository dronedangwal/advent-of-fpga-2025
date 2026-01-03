data = []

with open('input.txt') as f:
    data = f.read().strip().split('\n')
    # print(data)

pos = 50
ans = 0
for move in data:
    dir, clicks = move[0], int(move[1:])
    ans += clicks // 100
    clicks = clicks % 100
    if dir == 'L':
        if clicks >= pos and pos > 0:
            ans += 1
        pos -= clicks
        pos %= 100
    elif dir == 'R':
        if clicks >= (100 - pos) and pos > 0:
            ans += 1
        pos += clicks
        pos %= 100

print(ans)