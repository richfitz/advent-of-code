read_input <- function(path) {
  d <- as.integer(readLines(path))
  vapply(split(d, cumsum(is.na(d))), sum, numeric(1), na.rm = TRUE)
}

part1 <- function(d) {
  max(d)
}

part2 <- function(d) {
  sum(sort(d, decreasing = TRUE)[1:3])
}

d <- read_input("test-input-d01.txt")
stopifnot(part1(d) == 24000)
stopifnot(part2(d) == 45000)

d <- read_input("input-d01.txt")
part1(d)
part2(d)
