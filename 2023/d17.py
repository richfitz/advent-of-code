from collections import defaultdict
from heapq import heappop, heappush
from math import inf

def parse_input(path):
    with open(path) as f:
        return [list(map(int, line.strip())) for line in f.readlines()]


def best_path(grid, min_steps, max_steps):
    move = {"w": (0, 1), "s": (1, 0), "e": (0, -1), "n": (-1, 0)}
    turn = {"w": ("n", "s"), "s": ("w", "e"), "e": ("s", "n"), "n": ("e", "w")}
    n = len(grid)
    dist = defaultdict(lambda: inf)
    heap = [(0, (0, 0, "w")), (0, (0, 0, "s"))]
    while heap:
        cost, (r, c, d) = heappop(heap)
        if r == n - 1 and c == n - 1:
            return cost
        if cost > dist[r, c, d]:
            continue
        for d_next in turn[d]:
            dr, dc = move[d_next]
            cost_next = cost
            for i in range(max_steps):
                r_next, c_next = r + dr * (i + 1), c + dc * (i + 1)
                if r_next < 0 or r_next >= n or c_next < 0 or c_next >= n:
                    break
                cost_next += grid[r_next][c_next]
                if i < min_steps: # 0, 1, 2 get rejected for min_steps = 3
                    continue
                p_next = (r_next, c_next, d_next)
                if cost_next < dist[p_next]:
                    dist[p_next] = cost_next
                    heappush(heap, (cost_next, p_next))


def part1(grid):
    return best_path(grid, 0, 3)


def part2(grid):
    return best_path(grid, 3, 10)


test = parse_input("test-input-d17.txt")
assert part1(test) == 102
assert part2(test) == 94

d = parse_input("input-d17.txt")
print(part1(d)) # 1260
print(part2(d)) # 1416
