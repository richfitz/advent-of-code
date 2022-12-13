re <- function(n) {
  paste(vapply(seq_len(n - 1), function(i) {
    sprintf("(.)(?!%s)", paste0("\\", seq_len(i), collapse = "|"))
  }, ""), collapse = "")
}

part1 <- function(d) {
  c(regexpr(re(4), d, perl = TRUE)) + 3
}

part2 <- function(d) {
  c(regexpr(re(14), d, perl = TRUE)) + 13
}

test <- readLines("test-input-d06.txt")
stopifnot(identical(part1(test), c(7, 5, 6, 10, 11)))
stopifnot(identical(part2(test), c(19, 23, 23, 29, 26)))

d <- readLines("input-d06.txt")
part1(d)
part2(d)
