from collections import defaultdict
from functools import reduce
import re


def parse_round(g):
    res = defaultdict(lambda : 0)
    for el in g.split(", "):
        n, col = el.split(" ")
        res[col[0]] += int(n)
    return res


def parse_game(x):
    id, game = x.split(": ")
    id = int(id[5:])
    game = [parse_round(g) for g in game.split("; ")]
    return id, game


def parse_input(path):
    with open(path) as f:
        return [parse_game(x) for x in f.readlines()]


def part1(d):
    tot = 0
    for id, game in d:
        if all(g["r"] <= 12 and g["g"] <= 13 and g["b"] <= 14 for g in game):
            tot += id
    return tot


def part2(d):
    def power(game):
        res = defaultdict(lambda : 0)
        for g in game:
            for col in g.keys():
                res[col] = max(res[col], g[col])
        r, g, b = res.values()
        return r * g * b
    return sum(power(game[1]) for game in d)
        

d = parse_input("test-input-d02.txt")
assert part1(d) == 8
assert part2(d) == 2286

d = parse_input("input-d02.txt")
print(part1(d))
print(part2(d))
