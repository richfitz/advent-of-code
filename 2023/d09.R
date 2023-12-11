parse_input <- function(path) {
  expand <- function(x) {
    ends <- integer(0)
    while (!all(x == 0)) {
      ends <- c(ends, x[c(1, length(x))])
      x <- diff(x)
    }
    matrix(ends, 2)
  }
  lapply(strsplit(readLines(path), " +"), function(x) expand(as.integer(x)))
}


part1 <- function(d) {
  sum(vapply(d, function(x) sum(x[2, ]), numeric(1)))
}


part2 <- function(d) {
  sum(vapply(d, function(x) Reduce(function(a, b) b - a, rev(x[1, ]), 0), 1))
}


test <- parse_input("test-input-d09.txt")
stopifnot(part1(test) == 114)
stopifnot(part2(test) == 2)

d <- parse_input("input-d09.txt")
part1(d)
part2(d)
