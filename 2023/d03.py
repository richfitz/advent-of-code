import re

def parse_input(path):
    with open(path) as f:
        d = f.read()
    n = d.find("\n") + 1
    numbers = []
    for it in re.finditer("([0-9]+)", d):
        start, end = it.span()
        numbers.append((start // n, start % n, end % n, int(d[start:end])))
    symbols = [[] for _ in range(n - 1)]
    for it in re.finditer("[^.0-9\n]", d):
        start = it.start()
        symbols[start // n].append((start % n, d[start]))
    return numbers, symbols


def part1(d):
    def test_number(x):
        row, col_start, col_end, value = x
        for r in range(max(0, row - 1), min(len(symbols), row + 2)):
            for c, _ in symbols[r]:
                if c >= col_start - 1 and c <= col_end:
                    return value
        return 0
    numbers, symbols = d
    return sum([test_number(x) for x in numbers])


# This is a bit gross as we loop through all the numbers for every "*"
# but it could be worse, really.
def part2(d):
    def test_times(r, c):
        pos = []
        for num in numbers:
            row, col_start, col_end, value = num
            if abs(row - r) <= 1 and c >= col_start - 1 and c <= col_end:
                pos.append(value)
        return pos[0] * pos[1] if len(pos) == 2 else 0
    tot = 0
    numbers, symbols = d
    for r in range(len(symbols)):
        for c, v in symbols[r]:
            if v == "*":
                tot += test_times(r, c)
    return tot


d = parse_input("test-input-d03.txt")
assert part1(d) == 4361
assert part2(d) == 467835

d = parse_input("input-d03.txt")
print(part1(d))
print(part2(d))
