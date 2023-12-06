import re

def read_input(path):
    with open(path) as f:
        return [x.strip() for x in f.readlines()]


def part1(d):
    num = re.compile("[0-9]")
    digits = [[i for i in x if num.match(i)] for x in d]
    return sum([int(x[0] + x[-1]) for x in digits])


def part2(d):
    n = "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"
    # SPOILER ALERT
    #
    # This is quite mean for day 1 I feel. We need non-overlapping
    # matches here to avoid the situation where our number matches
    # overlap. Of course there are not any of these in the test input,
    # which is particularly cruel. For example:
    #
    # 6dxnslkl3xqlnm965twonexxn
    #                  ^^^
    #                    ^^^
    #
    # Here, a naive regex (like the first one I used) would match
    # 'two' but consume the string and fail to match 'one'. Other
    # examples of this include 'eightwo' and 'oneight'. Using a
    # zero-width lookahead '(?=...)' here does the trick here,
    # allowing findall() to match overlapping groups.
    num = re.compile(f"(?=([0-9]|{'|'.join(n)}))")
    def parse(x):
        digits = num.findall(x)
        for i, v in enumerate(digits):
            if v in n:
                digits[i] = str(n.index(v) + 1)
        return int(digits[0] + digits[-1])
    return sum([parse(x) for x in d])


test1 = read_input("test-input-d01-1.txt")
assert part1(test1) == 142

test2 = read_input("test-input-d01-2.txt")
assert part2(test2) == 281

d = read_input("input-d01.txt")
print(part1(d))
print(part2(d))
