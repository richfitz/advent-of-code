def parse_input(path):
    ret = []
    with open(path) as f:
        for line in f.readlines():
            direction, n_steps, col = line.strip().split()
            ret.append((direction, int(n_steps), col[2:-1]))
    return ret


def area(directions):
    dr = {"U": -1, "D": 1, "L": 0, "R": 0}
    dc = {"U": 0, "D": 0, "L": -1, "R": 1}
    r, c = 0, 0
    points = [(r, c)]
    for direction, n_steps, _ in directions:
        r, c = r + dr[direction] * n_steps, c + dc[direction] * n_steps
        points.append((r, c))

    # Shoelace formula, we've seen this once already this
    # year. Alternatively could have done a seed-fill, on p1 at least.
    interior = abs(sum(y1 * x2 - y2 * x1 for (y1, x1), (y2, x2) in
                       zip(points, points[1:]))) // 2
    perimeter = sum(el[1] for el in directions)

    return interior + perimeter // 2 + 1


def part1(directions):
    return area(directions)


def part2(directions):
    fixed = [("RDLU"[int(x[-1])], int(x[:-1], 16), x) for _, _, x in directions]
    return area(fixed)


test = parse_input("test-input-d18.txt")
assert part1(test) == 62
assert part2(test) == 952408144115

d = parse_input("input-d18.txt")
print(part1(d))
print(part2(d))
