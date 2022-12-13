parse_input <- function(path) {
  d <- matrix(unlist(strsplit(readLines(path), " ")), ncol = 2, byrow = TRUE)
  list(them = match(d[, 1], c("A", "B", "C")) - 1L,
       us = match(d[, 2], c("X", "Y", "Z")) - 1L)
}

part1 <- function(d) {
  sum(((d$us - d$them + 1) %% 3) * 3 + (1:3)[d$us + 1])
}

part2 <- function(d) {
  sum(d$us * 3 + (d$them + d$us - 1) %% 3 + 1)
}

test <- parse_input("test-input-d02.txt")
stopifnot(part1(test) == 15)
stopifnot(part2(test) == 12)

d <- parse_input("input-d02.txt")
part1(d) # 8933
part2(d) # 11998
