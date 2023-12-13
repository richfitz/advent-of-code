def parse_line(line):
    damaged = {"?": None, ".": False, "#": True}
    a, b = line.strip().split(" ")
    return tuple(damaged[i] for i in a), tuple(int(i) for i in b.split(","))


def parse_input(path):
    with open(path) as f:
        return [parse_line(line) for line in f.readlines()]


def find_run(x, n, after):
    ret = []
    m = 0
    for i, v in enumerate(x):
        if v is not False:
            m += 1
            if m >= n and (i - n + 1) >= after:
                ret.append(i - n + 1)
        else:
            m = 0
    return ret


cache = {}
def possible_matches(a, b):
    key = (a, b)
    if key not in cache:
        cache[key] = possible_matches_(a, b)
    return cache[key]


def possible_matches_(a, b):
    if len(b) == 0:
        return 0 if any(a) else 1
    i, n = max(enumerate(b), key=lambda x: x[1])
    tot = 0
    tmp = find_run(a, n, sum(b[:i]) + i)
    for k in tmp:
        possible = (
            (k == 0 or a[k - 1] is not True) and
            (k == len(a) - n or a[k + n] is not True)
        )
        if possible:
            a1, a2 = a[:max(k - 1, 0)], a[k + n + 1:]
            b1, b2 = b[:i], b[i + 1:]
            tot += possible_matches(a1, b1) * possible_matches(a2, b2)
    return tot


def possible_matches2(a, b):
    a2 = (*(*a, None) * 4, *a)
    return possible_matches(a2, b * 5)


def part1(d):
    return sum(possible_matches(a, b) for a, b in d)


def part2(d):
    return sum(possible_matches2(a, b) for a, b in d)


def possible_matches_str(line, part2=False):
    a, b = parse_line(line)
    return possible_matches2(a, b) if part2 else possible_matches(a, b)


test = parse_input("test-input-d12.txt")
assert part1(test) == 21
assert part2(test) == 525152

d = parse_input("input-d12.txt")
print(part1(d))
print(part2(d)) # 8475948826693
