def parse_input(path):
    pairs = []
    with open(path) as f:
        for r, x in enumerate(f.readlines()):
            for c, v in enumerate(x):
                if v == "#":
                    pairs.append((r, c))

    seen_rows = set()
    seen_cols = set()
    for r, c in pairs:
        seen_rows.add(r)
        seen_cols.add(c)
    offset = (fn(seen_rows), fn(seen_cols))

    return pairs, offset


# Number of empty spots for each position; could be more efficient at
# the cost of more bookkeeping.
def fn(x):
    return [(i - len([el for el in x if el < i])) for i in range(max(x) + 1)]


def distances(d, n):
    pairs, offset = d
    pos = [(r + offset[0][r] * (n - 1), c + offset[1][c] * (n - 1))
           for r, c in pairs]
    dist = 0
    for i, x in enumerate(pos):
        for y in pos[i:]:
            dist += abs(y[0] - x[0]) + abs(y[1] - x[1])
    return dist


def part1(d):
    return distances(d, 2)


def part2(d):
    return distances(d, 1000000)


test = parse_input("test-input-d11.txt")
assert part1(test) == 374

assert distances(test, 2) == 374
assert distances(test, 10) == 1030
assert distances(test, 100) == 8410

d = parse_input("input-d11.txt")
print(part1(d))
print(part2(d))
