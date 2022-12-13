x <- as.integer(readLines("input-d01.txt"))
part1 <- prod(x[which(outer(x, x, "+") == 2020, TRUE)[2, ]])
part2 <- prod(x[which(outer(outer(x, x, "+"), x, "+") == 2020, TRUE)[2, ]])
message(sprintf("Part 1: %s\nPart 2: %s", part1, part2))
