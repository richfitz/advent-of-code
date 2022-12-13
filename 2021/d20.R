parse <- function(path) {
  txt <- readLines(path)
  rules <- strsplit(txt[[1]], NULL)[[1]] == "#"

  data <- strsplit(txt[-(1:2)], NULL)
  data <- matrix(unlist(data) == "#", nrow = length(data), byrow = TRUE)
  list(data = embed(data, FALSE), rules = rules)
}


embed <- function(data, background) {
  nr <- nrow(data) + 2
  nc <- ncol(data) + 2
  ret <- matrix(background, nr, nc)
  ret[2:(nr - 1), 2:(nc - 1)] <- data
  ret
}


show <- function(data) {
  s <- array(ifelse(data, "#", "."), dim(data))
  cat(paste0(apply(s, 1, paste, collapse = ""), "\n", collapse = ""))
}


enhance <- function(x, rules) {
  data <- x$data
  background <- x$background

  data2 <- embed(data, background)
  rr <- outer(rep(0:2, 3), c(row(data)), "+")
  cc <- outer(rep(0:2, each = 3), c(col(data)), "+")
  idx <- (cc - 1) * nrow(data2) + rr
  ## n <- 2^(9 - seq_len(9))[c(matrix(1:9, 3, byrow = TRUE))]
  n <- c(256, 32, 4, 128, 16, 2, 64, 8, 1)
  code <- colSums(n * array(data2[c(idx)], dim(idx)))

  background <- rules[[sum(background * n) + 1]]
  list(data = embed(array(rules[code + 1L], dim(data)), background),
       background = background)
}


enhance_n <- function(x, n, rules) {
  for (i in seq_len(n)) {
    x <- enhance(x, rules)
  }
  x
}


part1 <- function(d) {
  rules <- d$rules
  x <- list(data = d$data, background = FALSE)
  sum(enhance_n(x, 2, rules)$data)
}


part2 <- function(d) {
  rules <- d$rules
  x <- list(data = d$data, background = FALSE)
  sum(enhance_n(x, 50, rules)$data)
}


d <- parse("test-input-d20.txt")
stopifnot(part1(d) == 35)
stopifnot(part2(d) == 3351)

d <- parse("input-d20.txt")
part1(d)
part2(d)
