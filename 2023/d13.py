def parse_map(s):
    rocks = []
    for r, line in enumerate(s.split("\n")):
        for c, ch in enumerate(line):
            if ch == "#":
                rocks.append((r, c))
    return rocks, (r + 1, max(c for _, c in rocks) + 1)


def parse_input(path):
    with open(path) as f:
        return [parse_map(s) for s in f.read().strip().split("\n\n")]


def can_reflect(at, i, m, dim, n_smudge):
    n = min(at + 1, dim[i] - at - 1)
    seen = set()
    for p in m:
        a = at - p[i] if p[i] <= at else p[i] - at - 1
        if a < n:
            x = (a, p[1 - i])
            seen.remove(x) if x in seen else seen.add(x)
    return len(seen) == n_smudge


def score(rocks, n_smudge):
    m, dim = rocks
    for i in range(2):
        for at in range(dim[i] - 1):
            if can_reflect(at, i, m, dim, n_smudge):
                return (100 if i == 0 else 1) * (at + 1)


def part1(d):
    return sum(score(rocks, 0) for rocks in d)


def part2(d):
    return sum(score(rocks, 1) for rocks in d)


test = parse_input("test-input-d13.txt")
assert part1(test) == 405
assert part2(test) == 400

d = parse_input("input-d13.txt")
print(part1(d))
print(part2(d))
