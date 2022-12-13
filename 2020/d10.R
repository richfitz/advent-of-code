read <- function(path) {
  x <- sort(as.integer(readLines(path)))
  c(0, x, x[[length(x)]] + 3)
}

f <- function(x) {
  r <- rle(diff(x))
  prod(c(1, 2, 4, 7)[r$lengths[r$values == 1]])
}

f(read("test-input-d101.txt"))
f(read("test-input-d102.txt"))

part1 <- prod(table(diff(read("input-d10.txt"))))
part2 <- format(f(read("input-d10.txt")), scientific = FALSE)
message(sprintf("Part 1: %s\nPart 2: %s", part1, part2))
