read <- function(path) {
  as.integer(readLines(path))
}


part1 <- function(x) {
  sum(diff(x) > 0)
}


part2 <- function(x) {
  i <- seq(1, length(x) - 2)
  sum(diff(rowSums(cbind(x[i], x[i + 1], x[i + 2]))) > 0)
}

test <- read("test-input-d01.txt")
stopifnot(part1(test) == 7)
stopifnot(part2(test) == 5)

input <- read("input-d01.txt")
part1(input)
part2(input)
