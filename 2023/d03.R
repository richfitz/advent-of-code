parse_input <- function(path) {
  d <- readLines(path)
  n <- nchar(d[[1]]) + 1L
  s <- paste(d, collapse = "\n")
  start <- gregexpr("([0-9]+)", s)[[1]] - 1L
  end <- start + attr(start, "match.length") - 1L
  value <- as.integer(substr(rep(s, length(start)), start + 1, end + 1))
  numbers <- cbind(row = start %/% n + 1,
                   col0 = start %% n + 1,
                   col1 = end %% n + 1)
  i <- gregexpr("[^.0-9\n]", s)[[1]] - 1L
  symbols <- cbind(row = i %/% n + 1,
                   col = i %% n + 1)

  near <- abs(outer(numbers[, 1], symbols[, 1], "-")) < 2 &
    (abs(outer(numbers[, 2], symbols[, 2], "-")) < 2 |
     abs(outer(numbers[, 3], symbols[, 2], "-")) < 2)
  colnames(near) <- substr(rep(s, length(i)), i + 1, i + 1)

  list(numbers = value, near = near)
}


part1 <- function(d) {
  sum(d$numbers[apply(d$near, 1, any)])
}


part2 <- function(d) {
  near <- d$near[, colnames(d$near) == "*"]
  sum(apply(near, 2, function(i) if (sum(i) == 2) prod(d$numbers[i]) else 0))
}

test <- parse_input("test-input-d03.txt")
stopifnot(part1(test) == 4361)
stopifnot(part2(test) == 467835)

d <- parse_input("input-d03.txt")
part1(d)
part2(d)
