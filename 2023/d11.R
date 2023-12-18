parse_input <- function(path) {
  m <- strsplit(readLines(path), NULL)
  pairs <- which(matrix(unlist(m) == "#", length(m), byrow = TRUE), TRUE)
  offset <- apply(pairs, 2, function(x) {
    cumsum(c(FALSE, diff(findInterval(seq_len(max(x)), sort(unique(x)))) == 0))
  })
  list(pairs = pairs,
       offset = cbind(offset[pairs[, 1], 1], offset[pairs[, 2], 2]))
}

distances <- function(d, n) {
  sum(dist(d$pairs + d$offset * (n - 1), "manhattan"))
}

part1 <- function(d) {
  distances(d, 2)
}

part2 <- function(d) {
  distances(d, 1000000)
}

test <- parse_input("test-input-d11.txt")
stopifnot(part1(test) == 374)

stopifnot(distances(test, 2) == 374)
stopifnot(distances(test, 10) == 1030)
stopifnot(distances(test, 100) == 8410)

d <- parse_input("input-d11.txt")
part1(d)
part2(d)
