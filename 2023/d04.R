n_shared <- function(a, b) {
  length(intersect(a, b))
}


parse_input <- function(path) {
  d <- strsplit(readLines(path), " +")
  i <- which(d[[1]] == "|")
  vapply(d, function(x) n_shared(x[3:(i - 1)], x[(i + 1):length(x)]), 1L)
}

part1 <- function(d) {
  sum((d > 0) * 2^(d - 1))
}

part2 <- function(d) {
  count <- rep(1L, length(d))
  for (i in which(d > 0)) {
    j <- seq(i + 1L, i + d[[i]])
    count[j] <- count[j] + count[[i]]
  }
  sum(count)
}

test <- parse_input("test-input-d04.txt")
stopifnot(part1(test) == 13)
stopifnot(part2(test) == 30)

d <- parse_input("input-d04.txt")
part1(d)
part2(d)
