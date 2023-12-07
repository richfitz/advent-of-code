def parse_input(path):
    with open(path) as f:
        return [x.strip().split() for x in f.readlines()]


def index_of_max(x):
    return x.index(max(x))


def parse_hand(hand, order):
    values = [12 - order.index(card) for card in hand]
    counts = [0] * len(order)
    for i in values:
        counts[i] += 1
    if order[12] == "J" and counts[0] > 0 and counts[1] < 5:
        counts[index_of_max(counts[1:]) + 1] += counts[0]
        counts[0] = 0
    return sorted(counts, reverse=True) + values


def score(d, order):
    res = sorted([(parse_hand(hand, order), bid) for hand, bid in d])
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
