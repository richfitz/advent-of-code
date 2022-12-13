parse <- function(txt) {
  as.integer(strsplit(txt, ",")[[1]])
}


## Naive simulation, iterate the system; this does not work for large
## numbers if iterations because we will grow the vectors to billions
## of elements.
simulate_naive <- function(x, n) {
  for (i in seq_len(n)) {
    x <- x - 1L
    i <- x < 0
    if (any(i)) {
      x[i] <- 6L
      x <- c(x, rep(8L, sum(i)))
    }
  }
  x
}


## More scalable version using a rotating table as a state.
simulate <- function(x, n) {
  age <- as.numeric(tabulate(x + 1L, 9))
  for (i in seq_len(n)) {
    new <- age[[1L]]
    age <- c(age[-1L], new)
    age[[7L]] <- age[[7L]] + new
  }
  age
}


part1 <- function(x) {
  sum(simulate(x, 80))
}


part2 <- function(x) {
  ## Need to return a string here so that we don't end up with
  ## scientific notation being used.
  format(sum(simulate(x, 256)), scientific = FALSE)
}


dat <- parse("3,4,3,1,2")
stopifnot(length(simulate_naive(dat, 18)) == 26)
stopifnot(sum(simulate(dat, 18)) == 26)

stopifnot(part1(dat) == 5934)
stopifnot(part2(dat) == "26984457539")

dat <- parse(readLines("input-d06.txt"))
part1(dat)
part2(dat)
