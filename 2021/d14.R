parse <- function(path) {
  txt <- readLines(path)

  rules <- matrix(unlist(strsplit(txt[-(1:2)], " -> ")), ncol = 2, byrow = TRUE)
  nms <- rules[, 1]

  ## The pair -> pair transition matrix
  to1 <- match(paste0(substr(nms, 1, 1), rules[, 2]), nms)
  to2 <- match(paste0(rules[, 2], substr(nms, 2, 2)), nms)
  transition <- matrix(0, length(nms), length(nms))
  transition[cbind(seq_along(nms), to1)] <- 1
  transition[cbind(seq_along(nms), to2)] <- 1

  ## Starting point as a vector of pairs
  start <- txt[[1]]
  n <- nchar(start)
  value <- table(factor(substr(rep(start, n - 1), seq_len(n - 1), 2:n), nms))

  list(value = value, transition = transition,
       score = substr(nms, 1, 1), last = substr(start, n, n))
}


run <- function(d, n) {
  value <- d$value
  for (i in seq_len(n)) {
    value <- value %*% d$transition
  }
  total <- tapply(value, d$score, sum)
  total[[d$last]] <- total[[d$last]] + 1L
  total
}


part1 <- function(d) {
  diff(range(run(d, 10)))
}


part2 <- function(d) {
  format(diff(range(run(d, 40))), digits = 12)
}


## Some values from the test input:
cmp <- c("NCNBCHB",
         "NBCCNBBBCBHCB",
         "NBBBCNCCNBBNBNBBCHBHHBCHB",
         "NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB")
n <- vapply(cmp, function(x) table(strsplit(x, NULL)[[1]]), integer(4),
            USE.NAMES = FALSE)

d <- parse("test-input-d14.txt")
stopifnot(run(d, 1) == n[, 1])
stopifnot(run(d, 2) == n[, 2])
stopifnot(run(d, 3) == n[, 3])
stopifnot(run(d, 4) == n[, 4])
stopifnot(part1(d) == 1588)
stopifnot(part2(d) == "2188189693529")

d <- parse("input-d14.txt")
part1(d)
part2(d)
