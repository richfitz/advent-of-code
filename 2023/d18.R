parse_input <- function(path) {
  d <- matrix(unlist(strsplit(readLines(path), " ")), ncol = 3, byrow = TRUE)
  list(list(dir = d[, 1],
            n = as.integer(d[, 2])),
       list(dir = chartr("0123", "RDLU", substr(d[, 3], 8, 8)),
            n = strtoi(substr(d[, 3], 3, 7), 16)))
}

area <- function(d) {
  x <- c(0, cumsum(unname(c(U = -1, D = 1, L = 0, R = 0)[d$dir]) * d$n))
  y <- c(0, cumsum(unname(c(U = 0, D = 0, L = -1, R = 1)[d$dir]) * d$n))
  # Shoelace formula:
  abs(sum(x * c(y[-1], y[1]) - c(x[-1], x[1]) * y) / 2) + sum(d$n) / 2 + 1
}

part1 <- function(d) {
  area(d[[1]])
}

part2 <- function(d) {
  format(area(d[[2]]), digits = 20)
}

test <- parse_input("test-input-d18.txt")
stopifnot(part1(test) == 62)
stopifnot(part2(test) == "952408144115")

d <- parse_input("input-d18.txt")
part1(d)
part2(d)
