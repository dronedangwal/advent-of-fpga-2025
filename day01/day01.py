with open('input.txt') as f:
    data = f.read().strip().split('\n')

pos = 50
p1 = 0; p2 = 0

for move in data:
    dir, clicks = move[0], int(move[1:])
    p2 += clicks // 100
    clicks %= 100
    if dir == 'L':
        p2 += int((pos > 0) & (clicks >= pos))
    elif dir == 'R':
        p2 += int((pos > 0) & (clicks + pos >= 100))
    pos += clicks if dir == 'R' else -clicks
    pos %= 100
    p1 += pos == 0

print(f"Part 1 password: {p1}")
print(f"Part 2 password: {p2}")
