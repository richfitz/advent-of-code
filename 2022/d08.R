parse_input <- function(path) {
  txt <- readLines(path)
  matrix(as.integer(unlist(strsplit(txt, NULL))), length(txt), byrow = TRUE)
}

look1 <- function(x, reverse = FALSE) {
  ret <- rep(FALSE, length(x))
  m <- -1L
  for (i in if (reverse) rev(seq_along(x)) else seq_along(x)) {
    if (ret[[i]] <- x[[i]] > m) {
      m <- x[[i]]
    }
  }
  ret
}

look2 <- function(x, v) {
  for (i in seq_along(v)) {
    if (v[[i]] >= x) {
      return(i)
    }
  }
  length(v)
}

part1 <- function(m) {
  sum(t(apply(m, 1, look1) | apply(m, 1, look1, TRUE)) |
      apply(m, 2, look1) | apply(m, 2, look1, TRUE))
}

part2 <- function(m) {
  ret <- 0
  n <- nrow(m)
  for (i in 2:(n - 1)) {
    for (j in 2:(n - 1)) {
      x <- m[i, j]
      ret <- max(ret, look2(x, m[(i - 1):1, j]) * look2(x, m[(i + 1):n, j]) *
                      look2(x, m[i, (j - 1):1]) * look2(x, m[i, (j + 1):n]))
    }
  }
  ret
}

test <- parse_input("test-input-d08.txt")
stopifnot(part1(test) == 21)
stopifnot(part2(test) == 8)

d <- parse_input("input-d08.txt")
part1(d)
part2(d)
