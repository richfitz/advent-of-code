from operator import add, sub

move = {
    "north": (-1,  0),
    "south": ( 1,  0),
    "east":  ( 0,  1),
    "west":  ( 0, -1)
}

turn = {
    ("north", "|"): "north",
    ("north", "F"): "east",
    ("north", "7"): "west",
    ("south", "|"): "south",
    ("south", "L"): "east",
    ("south", "J"): "west",
    ("east",  "-"): "east",
    ("east",  "J"): "north",
    ("east",  "7"): "south",
    ("west",  "-"): "west",
    ("west",  "L"): "north",
    ("west" , "F"): "south"
}


def parse_input(path):
    with open(path) as f:
        s = f.read()
    m = s.strip().split("\n")
    i = s.index("S")
    n = len(m[0]) + 1
    loop = find_loop((i // n, i % n), m)
    return loop, clean_map(loop, m)


def find_loop(start, m):
    for direction in ["east", "west", "north", "south"]:
        try:
            pos, loop = start, []
            while True:
                loop.append(pos)
                pos = tuple(map(add, pos, move[direction]))
                if pos == start:
                    return loop
                direction = turn[(direction, m[pos[0]][pos[1]])]
        except KeyError: # fail to look up a turn, this is junk
            pass


def clean_map(loop, m):
    ret = [[None] * len(m[0]) for _ in m]
    for r, c in loop:
        ret[r][c] = m[r][c]
    start = { # we don't actually see all of these in practice!
        (0, 2): "-", (0, -2): "-", (2, 0): "|", (-2, 0): "|",
        (-1, 1): "F", (-1, -1): "7", (1, -1): "J", (1, 1): "L"
    }
    ret[loop[0][0]][loop[0][1]] = start[tuple(map(sub, loop[1], loop[-1]))]
    return ret


def part1(d):
    return int(len(d[0]) / 2)


def part2(d):
    n_inside = 0
    for row in d[1]:
        is_inside = False
        for ch in row:
            if ch is None:
                n_inside = n_inside + int(is_inside)
            elif ch == "|":
                is_inside = not is_inside
            elif ch == "F" or ch == "L":
                start = ch
            elif (ch == "7" and start == "L") or (ch == "J" and start == "F"):
                is_inside = not is_inside
    return n_inside


assert part1(parse_input("test-input-d10-1.txt")) == 4
assert part1(parse_input("test-input-d10-2.txt")) == 4
assert part1(parse_input("test-input-d10-3.txt")) == 8
assert part1(parse_input("test-input-d10-4.txt")) == 8
assert part2(parse_input("test-input-d10-5.txt")) == 4
assert part2(parse_input("test-input-d10-6.txt")) == 8
assert part2(parse_input("test-input-d10-7.txt")) == 10

d = parse_input("input-d10.txt")
print(part1(d))
print(part2(d))
