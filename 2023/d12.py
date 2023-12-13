from functools import lru_cache # older python too

def parse_input(path):
    def parse_line(line):
        a, b = line.strip().split(" ")
        return a, tuple(int(i) for i in b.split(","))
    with open(path) as f:
        return [parse_line(line) for line in f.readlines()]


@lru_cache(maxsize=None)
def possible_matches(a, b):
    if len(b) == 0:
        return int(a.find("#") < 0)
    len_a = len(a)
    b0 = b[0]
    b_tail = b[1:]
    tot = 0
    for i in range(len_a - sum(b) - len(b) + 2):
        if i > 0 and a[i - 1] == "#":
            break
        elif any(x == "." for x in a[i:i + b0]):
            continue
        elif i + b0 == len_a or a[i + b0] != "#":
            tot += possible_matches(a[i + b0 + 1:], b_tail)
    return tot


def part1(d):
    return sum(possible_matches(a, b) for a, b in d)


def part2(d):
    return sum(possible_matches("?".join([a for _ in range(5)]), b * 5)
               for a, b in d)


test = parse_input("test-input-d12.txt")
assert part1(test) == 21
assert part2(test) == 525152

d = parse_input("input-d12.txt")
print(part1(d))
print(part2(d))
