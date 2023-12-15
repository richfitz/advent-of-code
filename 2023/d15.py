def parse_input(path):
    with open(path) as f:
        return f.read().strip().split(",")
    

def hash1a(string):
    value = 0
    for char in string:
        value = ((value + ord(char)) * 17) % 256
    return value


def part1(d):
    return sum(hash1a(x) for x in d)


def part2(d):
    boxes = [{} for _ in range(256)]
    for s in d:
        box = boxes[hash1a(s[:-1 if s.endswith("-") else -2])]
        if s[-1] == "-":
            s_label = s[:-1]
            if s_label in box:
                del box[s_label]
        else:
            s_split = s.split("=")
            box[s_split[0]] = int(s_split[1])
    return sum((i + 1) * sum((i + 1) * v for i, v in enumerate(b.values()))
               for i, b in enumerate(boxes))


assert hash1a("rn=1") == 30
assert hash1a("cm-") == 253
test = parse_input("test-input-d15.txt")
assert part1(test) == 1320
assert part2(test) == 145

d = parse_input("input-d15.txt")
print(part1(d))
print(part2(d))
