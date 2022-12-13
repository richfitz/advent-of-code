parse <- function(path) {
  txt <- readLines(path)
  matrix(as.integer(unlist(strsplit(txt, "[^0-9]+"))) + 1L,
         ncol = 4, byrow = TRUE)
}


locate <- function(dat) {
  m <- matrix(0L, max(dat), max(dat))
  for (row in seq_len(nrow(dat))) {
    i <- seq(dat[row, 2], dat[row, 4])
    j <- seq(dat[row, 1], dat[row, 3])
    k <- cbind(i, j)
    m[k] <- m[k] + 1L
  }
  m
}


part1 <- function(dat) {
  dat <- dat[dat[, 1] == dat[, 3] | dat[, 2] == dat[, 4], ]
  sum(locate(dat) >= 2)
}


part2 <- function(dat) {
  sum(locate(dat) >= 2)
}


dat <- parse("test-input-d05.txt")
stopifnot(part1(dat) == 5)
stopifnot(part2(dat) == 12)


dat <- parse("input-d05.txt")
part1(dat)
part2(dat)
