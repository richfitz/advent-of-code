import math


def lcm(x):
    ret = x[0]
    for el in x[1:]:
        ret = abs(el * ret) // math.gcd(el, ret)
    return ret


def parse_input(path):
    with open(path) as f:
        d = f.readlines()
    directions = [1 if x == "R" else 0 for x in d[0].strip()]
    nodes = {x[:3]: (x[7:10], x[12:15]) for x in d[2:]}
    return directions, nodes


def part1(d):
    directions, nodes = d
    i = 0
    pos = "AAA"
    while pos != "ZZZ":
        pos = nodes[pos][directions[i % len(directions)]]
        i += 1
    return i


def part2(d):
    directions, nodes = d
    pos = tuple(x for x in nodes if x[2] == "A")
    i = 0
    res = [[] for _ in pos]
    while True:
        direction = directions[i % len(directions)]
        pos = tuple(nodes[x][direction] for x in pos)
        for j, v in enumerate(pos):
            if v[2] == "Z":
                res[j].append(i)
        if all(len(x) >= 2 for x in res):
            return lcm([x[1] - x[0] for x in res])
        i += 1
    return i


test1 = parse_input("test-input-d08-1.txt")
assert part1(test1) == 2

test2 = parse_input("test-input-d08-2.txt")
assert part2(test2) == 6

d = parse_input("input-d08.txt")
print(part1(d))
print(part2(d))
