def parse_input(path):
    with open(path) as f:
        d = [x.split() for x in f.readlines()]
    i = d[0].index("|")
    return [len(set(el[2:i]).intersection(el[i + 1:])) for el in d]


def part1(d):
    return(sum(0 if n == 0 else 2 ** (n - 1) for n in d))


def part2(d):
    count = [1 for _ in d]
    for i in range(len(d)):
        for j in range(i + 1, min(i + d[i] + 1, len(d))):
            count[j] += count[i]
    return sum(count)


d = parse_input("test-input-d04.txt")
assert part1(d) == 13
assert part2(d) == 30

d = parse_input("input-d04.txt")
print(part1(d))
print(part2(d))
