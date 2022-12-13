parse <- function(path) {
  txt <- readLines(path)
  matrix(as.integer(unlist(strsplit(txt, NULL))), length(txt), byrow = TRUE)
}


bin_to_int <- function(x) {
  sum(x * 2^(length(x) - seq_along(x)))
}


part1 <- function(dat) {
  gamma <- round(colMeans(dat))
  epsilon <- 1 - gamma
  bin_to_int(gamma) * bin_to_int(epsilon)
}


common <- function(x, most_common) {
  n1 <- sum(x == 1)
  as.integer(xor(most_common, n1 < length(x) - n1))
}


filter <- function(dat, most_common) {
  for (i in seq_len(ncol(dat))) {
    dat <- dat[dat[, i] == common(dat[, i], most_common), , drop = FALSE]
    if (nrow(dat) == 1) {
      break
    }
  }
  stopifnot(nrow(dat) == 1)
  dat[1, ]
}


part2 <- function(dat) {
  bin_to_int(filter(dat, TRUE)) * bin_to_int(filter(dat, FALSE))
}


dat <- parse("test-input-d03.txt")
stopifnot(part1(dat) == 198)
stopifnot(part2(dat) == 230)


dat <- parse("input-d03.txt")
part1(dat)
part2(dat)
