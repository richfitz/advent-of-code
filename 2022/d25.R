parse_snafu <- function(x) {
  v <- strsplit(x, NULL)[[1]]
  sum(5^((length(v) - 1):0) * (match(v, c("=", "-", "0", "1", "2")) - 3))
}

human_to_snafu <- function(x) {
  m <- floor(log(x, 5)) + 1
  n <- 5^(m:0)
  v <- x %% (n * 5) %/% n
  for (i in rev(seq_along(v))) {
    if (v[[i]] > 2) {
      tmp <- v[[i]] * n[[i]] + v[[i - 1]] * n[[i - 1]]
      v[[i - 1]] <- ceiling(tmp / n[[i - 1]])
      v[[i]] <- (tmp - v[[i - 1]] * n[[i - 1]]) / n[[i]]
    }
  }
  sub("^0+", "", paste(c("=", "-", "0", "1", "2")[v + 3], collapse = ""))
}


part1 <- function(d) {
  human_to_snafu(sum(vapply(d, parse_snafu, 1, USE.NAMES = FALSE)))
}

stopifnot(part1(readLines("test-input.txt")) == "2=-1=0")

part1(readLines("input-d25.txt"))
