def parse_input(path):
    with open(path) as f:
        return [x.strip() for x in f.readlines()]

horizontal = { "N": False, "S": False, "E": True, "W": True }
forwards = { "N": False, "E": False, "S": True, "W": True }
reflect = {
    "/":  { "W": "N", "E": "S", "N": "W", "S": "E" },
    "\\": { "W": "S", "E": "N", "N": "E", "S": "W" }
}


def explore1(pos, grid, pending, seen):
    if pos in seen:
        return
    seen.add(pos)
    r, c, d = pos
    curr = grid[r][c]
    if curr == "-" and not horizontal[d]:
        pending.extend(((r, c, "E"), (r, c, "W")))
    elif curr == "|" and horizontal[d]:
        pending.extend(((r, c, "N"), (r, c, "S")))
    else:
        if curr == "/" or curr == "\\":
            d = reflect[curr][d]
        if horizontal[d]:
            c += 1 if forwards[d] else -1
            if c < 0 or c >= len(grid):
                return
        else:
            r += 1 if forwards[d] else -1
            if r < 0 or r >= len(grid):
                return
        return r, c, d


def explore(pos, grid):
    pending = [pos]
    seen = set()
    while pending:
        pos = pending.pop()
        while pos:
            pos = explore1(pos, grid, pending, seen)
    points = set((r, c) for r, c, _ in seen)
    return len(points)


def part1(d):
    return explore((0, 0, "W"), d)


def part2(d):
    ps = []
    n = len(d)
    for i in range(n):
        ps.extend(((0, i, "S"), (n - 1, i, "N"), (i, 0, "W"), (i, n - 1, "E")))
    return max(explore(p, d) for p in ps)


test = parse_input("test-input-d16.txt")
assert part1(test) == 46
assert part2(test) == 51

d = parse_input("input-d16.txt")
print(part1(d))
print(part2(d))
