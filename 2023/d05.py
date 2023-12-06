from operator import itemgetter


def parse_input(path):
    def parse_map(x):
        x = x.strip().split("\n")
        return sorted([[int(v) for v in el.split()] for el in x[1:]],
                      key=itemgetter(1))

    with open(path) as f:
        seeds_str = f.readline()
        seeds = [int(x) for x in seeds_str[7:-1].split()]
        maps = [parse_map(x) for x in f.read().split("\n\n")]

    return seeds, maps


def apply_map(x, m):
    for i in m:
        if x >= i[1] and x < i[1] + i[2]:
            return x - i[1] + i[0]
    return x


def part1(d):
    values, maps = d
    for m in maps:
        values = [apply_map(x, m) for x in values]
    return min(values)


def seed_ranges(seeds):
    ranges = [(x[0], x[0] + x[1] - 1) for x in zip(seeds[::2], seeds[1::2])]
    ranges.sort(key=itemgetter(0))
    return ranges


def apply_map2(ranges, m):
    ret = []
    for start, end in ranges:
        ret += apply_map_range(start, end, m)
    return sorted(ret, key=itemgetter(0))


#          [-------]
# 1. [..]
# 2.   [...xx]
# 3.        [xxxx]
# 4.    [...xxxxxxx........]
# 5.            [xxx...]
# 6.                     [...] (only after using up all maps)
def apply_map_range(start, end, m):
    ret = []
    for m_to, m_start, m_length in m:
        m_end = m_start + m_length - 1
        m_offset = m_to - m_start
        if end < m_start: # option 1
            ret.append((start, end))
            break
        # below here we assume end >= m_start
        elif start < m_start and end <= m_end: # option 2
            ret.append((start, m_start - 1))
            ret.append((m_start + m_offset, end + m_offset))
            break
        elif start >= m_start and end <= m_end: # option 3
            ret.append((start + m_offset, end + m_offset))
            break
        elif start < m_start and end > m_end: # option 4
            ret.append((start, m_start - 1))
            ret.append((m_start + m_offset, m_end + m_offset))
            start = m_end + 1
        elif start >= m_start and start <= m_end and end > m_end: # option 5
            ret.append((start + m_offset, m_end + m_offset))
            start = m_end + 1
        else: # option 6, perhaps
            assert start > m_end

    if start > m_end: # option 6, indeed
        ret.append((start, end))

    return ret


def part2(d):
    seeds, maps = d
    ranges = seed_ranges(seeds)
    for i, m in enumerate(maps):
        ranges = apply_map2(ranges, m)
    return ranges[0][0]


m = [(50, 98, 2), (52, 50, 48)]
assert apply_map(79, m) == 81
assert apply_map(14, m) == 14
assert apply_map(55, m) == 57
assert apply_map(13, m) == 13

test = parse_input("test-input-d05.txt")
assert part1(test) == 35
assert part2(test) == 46

d = parse_input("input-d05.txt")
print(part1(d))
print(part2(d))
