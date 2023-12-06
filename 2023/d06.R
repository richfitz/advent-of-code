parse_input <- function(path) {
  d <- strsplit(sub(".+: *", "", readLines(path)), " +")
  list(sapply(d, as.numeric),
       sapply(d, function(x) as.numeric(paste(x, collapse = ""))))
}

options <- function(t, r) {
  disc <- sqrt(t * t - 4 * (r + 1))
  floor((t + disc) / 2) - ceiling((t - disc) / 2) + 1
}


part1 <- function(d) {
  prod(options(d[[1]][, 1], d[[1]][, 2]))
}


part2 <- function(d) {
  options(d[[2]][[1]], d[[2]][[2]])
}


test <- parse_input("test-input-d06.txt")
stopifnot(part1(test) == 288)
stopifnot(part2(test) == 71503)

d <- parse_input("input-d06.txt")
part1(d)
part2(d)
