import operator

def parse_object(x):
    tmp = (el.split("=") for el in x[1:-1].split(","))
    return {k: int(v) for k, v in tmp}


def parse_instruction(x):
    rules = x[x.index("{") + 1:-1].split(",")
    data = [(e[0], e[1], int(e[2:e.index(":")]), e[e.index(":") + 1:])
            for e in rules[:-1]]
    data.append(rules[-1])
    return x[:x.index("{")], data


def parse_input(path):
    with open(path) as f:
        instructions, objects = f.read().strip().split("\n\n")
    rules = dict(parse_instruction(x) for x in instructions.split("\n"))
    rules["A"] = True
    rules["R"] = False
    return rules, [parse_object(x) for x in objects.split("\n")]


def apply_workflow(obj, rules):
    ops = {"<": operator.lt, ">": operator.gt}
    def f(w):
        for r in w[:-1]:
            k, op, value, result = r
            if ops[op](obj[k], value):
                return rules[result]
        return rules[w[-1]]
    w = rules["in"]
    while not isinstance(w, bool):
        w = f(w)
    return w


def part1(d):
    rules, objects = d
    return sum(sum(obj.values()) for obj in objects
               if apply_workflow(obj, rules))


def split_range(ranges, key, op, value):
    r = ranges[key]
    if op == "<":
        a = {k: (r[0], value-1) if k == key else v for k, v in ranges.items()}
        b = {k: (value, r[1]) if k == key else v for k, v in ranges.items()}
    else:
        a = {k: (value+1, r[1]) if k == key else v for k, v in ranges.items()}
        b = {k: (r[0], value) if k == key else v for k, v in ranges.items()}
    return a, b


def prod(v):
    ret = 1
    for el in v:
        ret *= el
    return ret


def part2(d):
    rules = d[0]
    ranges = {k: (1, 4000) for k in "xmas"}
    def f(ranges, w):
        total = 0
        if isinstance(w, bool):
            return w * prod(max(0, v[1] - v[0] + 1) for v in ranges.values())
        for r in w[:-1]:
            k, op, value, result = r
            ranges_descend, ranges = split_range(ranges, k, op, value)
            total += f(ranges_descend, rules[result])
        return total + f(ranges, rules[w[-1]])
    return f(ranges, rules["in"])


test = parse_input("test-input-d19.txt")
assert part1(test) == 19114
assert part2(test) == 167409079868000

d = parse_input("input-d19.txt")
print(part1(d))
print(part2(d))
