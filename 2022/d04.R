parse_input <- function(path) {
  matrix(as.integer(unlist(strsplit(readLines(path), "[^0-9]"))),
         ncol = 4, byrow = TRUE)
}

part1 <- function(d) {
  sum((d[, 1] <= d[, 3] & d[, 2] >= d[, 4]) |
      (d[, 1] >= d[, 3] & d[, 2] <= d[, 4]))
}

part2 <- function(d) {
  sum((d[, 1] <= d[, 4] & d[, 2] >= d[, 3]))
}

test <- parse_input("test-input-d04.txt")
stopifnot(part1(test) == 2)
stopifnot(part2(test) == 4)

d <- parse_input("input-d04.txt")
part1(d)
part2(d)
