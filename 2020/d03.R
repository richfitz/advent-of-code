f <- function(m, nd = 1, nr = 3) {
  i <- seq(1, by = nd, to = nrow(m))
  j <- (seq(0, by = nr, length.out = length(i)) %% ncol(m)) + 1
  sum(m[cbind(i, j)] == "#")
}

m <- do.call(rbind, strsplit(readLines("input-d03.txt"), NULL))
part1 <- f(m)
part2 <- prod(c(f(m, 1, 1), f(m, 1, 3), f(m, 1, 5), f(m, 1, 7), f(m, 2, 1)))
message(sprintf("Part 1: %s\nPart 2: %s", part1, part2))
