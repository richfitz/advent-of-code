from collections import defaultdict


def parse_input(path):
    ret = []
    with open(path) as f:
        for x in f.readlines():
            ret.append(x.strip().split())
    return ret


def parse_hand(hand, order):
    values = [order.index(card) for card in hand]
    table = defaultdict(lambda : 0)
    for i in values:
        table[i] += 1
    res = sorted(table.items(), key=lambda x: (-x[1], x[0]))
    if order[12] != "J" or table[12] == 0 or table[12] == 5:
        best = res[0]
    else:
        best = res[0] if len(res) == 1 or res[0][0] != 12 else res[1]
        best = best[0], best[1] + table[12]
        table[12] = 0
    n = len([x for x in table.values() if x > 0])
    if n == 1:
        score = 0
    elif n == 2: # full house or four of a kind
        score = 1 + int(best[1] != 4)
    elif n == 3: # three of a kind or two pair
        score = 3 + int(best[1] != 3)
    else:        # two of a kind or a big pile of nothing
        score = 5 + int(best[1] != 2)
    return [score, *values, hand]


def score(d, order):
    res = [(parse_hand(hand, order), bid) for hand, bid in d]
    res.sort(key=lambda x: x[0], reverse=True)
    return sum((i + 1) * int(x[1]) for i, x in enumerate(res))


def part1(d):
    order = ["A", "K", "Q", "J", "T", "9", "8", "7", "6", "5", "4", "3", "2"]
    return score(d, order)


def part2(d):
    order = ["A", "K", "Q", "T", "9", "8", "7", "6", "5", "4", "3", "2", "J"]
    return score(d, order)


test = parse_input("test-input-d07.txt")
assert part1(test) == 6440
assert part2(test) == 5905

d = parse_input("input-d07.txt")
print(part1(d))
print(part2(d))
