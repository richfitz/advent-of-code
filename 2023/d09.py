from functools import reduce


def parse_input(path):
    with open(path) as f:
        return [expand(line) for line in f.readlines()]


def expand(line):
    x = [int(x) for x in line.strip().split()]
    front, back = [x[0]], [x[-1]]
    while not all(i == 0 for i in x):
        x = [l - r for r, l in zip(iter(x), iter(x[1:]))]
        front.append(x[0])
        back.append(x[-1])
    return front, back


def part1(d):
    return sum(sum(back) for _, back in d)


def part2(d):
    return sum(reduce(lambda a, b: b - a, reversed(front)) for front, _ in d)


test = parse_input("test-input-d09.txt")
assert part1(test) == 114
assert part2(test) == 2

d = parse_input("input-d09.txt")
print(part1(d))
print(part2(d))
