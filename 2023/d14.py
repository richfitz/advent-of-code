def parse_input(path):
    rocks = []
    with open(path) as f:
        for r, s in enumerate(f.readlines()):
            for c, x in enumerate(s.strip()):
                if x != ".":
                    rocks.append([r, c, x == "O"])
    return r + 1, rocks


def to_str(n, rocks):
    res = [["." for _ in range(n)] for _ in range(n)]
    for r, c, moves in rocks:
        res[r][c] = "O" if moves else "#"
    return "\n".join("".join(r) for r in res)


def tilt(n, rocks, direction):
    idx = 0 if direction == "north" or direction == "south" else 1
    down = direction == "north" or direction == "west"
    rocks = sorted(rocks, key=lambda x: x[idx], reverse=not down)
    base = [0 if down else n - 1] * n
    for p in rocks:
        i = p[idx]
        j = p[1 - idx]
        if p[2]: # does it move?
            p[idx] = base[j]
            base[j] += (1 if down else -1)
        else:
            base[j] = i + (1 if down else -1)


def pattern_id(n, rocks):
    return tuple(sorted(r * n + c for r, c, it_moves in rocks if it_moves))


def score(n, rocks):
    return sum(n - r for r, _, it_moves in rocks if it_moves)


def spin(n, rocks):
    for direction in ["north", "west", "south", "east"]:
        tilt(n, rocks, direction)
    return score(n, rocks)


def part1(n, rocks):
    rocks = [p.copy() for p in rocks]
    tilt(n, rocks, "north")
    return score(n, rocks)


def part2(n, rocks):
    rocks = [p.copy() for p in rocks]
    seen = {}
    load = []
    for i in range(1000000000):
        load.append(spin(n, rocks))
        pat = pattern_id(n, rocks)
        if pat not in seen:
            seen[pat] = i
        else:
            start = seen[pat]                    # (   period    )
            return load[(1000000000 - start - 1) % (i - seen[pat]) + start]


test = parse_input("test-input-d14.txt")
assert part1(*test) == 136
assert part2(*test) == 64

d = parse_input("input-d14.txt")
print(part1(*d))
print(part2(*d))
