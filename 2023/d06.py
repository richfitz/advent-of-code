import math

def parse_input(path):
    with open(path) as f:
        d = [line.split()[1:] for line in f.readlines()]
    return [[int(v) for v in x] for x in d], [int("".join(x)) for x in d]


# Solve options using quadratic formula:
def options(t, r):
    disc = math.sqrt(t * t - 4 * (r + 1))
    return math.floor((t + disc) / 2) - math.ceil((t - disc) / 2) + 1


def part1(d):
    ret = 1
    for t, r in zip(d[0][0], d[0][1]):
        ret *= options(t, r)
    return ret


def part2(d):
    return options(d[1][0], d[1][1])


test = parse_input("test-input-d06.txt")
assert part1(test) == 288
assert part2(test) == 71503

d = parse_input("input-d06.txt")
print(part1(d))
print(part2(d))
