parse_input <- function(path) {
  lapply(strsplit(readLines(path), NULL), match, c(letters, LETTERS))
}

part1 <- function(d) {
  f <- function(x) {
    i <- seq_len(length(x) / 2)
    intersect(x[i], x[-i])
  }
  sum(vapply(d, f, numeric(1)))
}

part2 <- function(d) {
  sum(apply(structure(d, dim = c(3, length(d) / 3)), 2, Reduce, f = intersect))
}

test <- parse_input("test-input-d03.txt")
stopifnot(part1(test) == 157)
stopifnot(part2(test) == 70)

d <- parse_input("input-d03.txt")
part1(d)
part2(d)
