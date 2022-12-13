parse <- function(path) {
  as.integer(gsub(".*: ", "", readLines(path)))
}


## This is very simple minded.  I thought of all sorts of ways of
## making this more clever but was undecided how part 2 would
## complicate the problem.  I was unprepared.
play_deterministic <- function(p1, p2) {
  dice <- matrix(1:300, 6)
  a <- colSums(dice[1:3, ])
  b <- colSums(dice[4:6, ])
  i <- s1 <- s2 <- 0L
  repeat {
    j <- (i %% length(a)) + 1L
    i <- i + 1L
    p1 <- (p1 + a[j] - 1L) %% 10 + 1L
    s1 <- s1 + p1
    if (s1 >= 1000) {
      return(c(s2, i * 6 - 3))
    }
    p2 <- (p2 + b[j] - 1L) %% 10 + 1L
    s2 <- s2 + p2
    if (s2 >= 1000) {
      return(c(s1, i * 6))
    }
  }
}


## For the quantum dice game, model the state as a position * score
## matrix.  There are 22 interesting scores: 0 (the starting point),
## 1..20 (live scores) and 22 (winning scores).  We'll extract the
## winners as soon as they happen and advancing beyond this position
## is impossible.
play_quantum <- function(p1, p2) {
  n <- 22
  a <- b <- matrix(0, 10, n)
  a[p1, 1] <- b[p2, 1] <- 1
  state <- list(a, b)
  wins <- numeric(2)
  idx <- 1:2
  repeat {
    k <- idx[[1]]
    x <- update(state[[k]])
    if (any(x[, n] > 0)) {
      wins[k] <- wins[k] + sum(x[, n]) * sum(state[[idx[[2]]]])
      x[, n] <- 0
    }
    if (all(x == 0)) {
      break
    }
    state[[k]] <- x
    idx <- rev(idx)
  }
  wins
}


update <- function(state) {
  n <- ncol(state)
  m <- nrow(state)
  i <- which(colSums(state[, -n]) > 0)
  ## The position update is easy, but the spread over the grid is more
  ## complicated (it could probably expressed as a second matrix
  ## multiplication for the suitably enthusiastic)
  move <- transition %*% state[, i, drop = FALSE]

  new <- matrix(0, nrow(state), n)
  new[, n] <- state[, n]
  for (j in seq_along(i)) {
    ## The pmin here takes care of the winning absorbing state; we
    ## need to accumulate only for that entry, but might as well do it
    ## for everything here as otherwise the bookeeping would be worse.
    idx <- cbind(seq_len(m), pmin(seq_len(m) + i[j], n))
    new[idx] <- new[idx] + move[, j]
  }

  new
}


## Transition matrix for position->position.  This can certainly be
## done more tidily but this gets the job done.
transition <- local({
  n <- table(rowSums(expand.grid(1:3, 1:3, 1:3)))
  f <- function(i) {
    ret <- numeric(10)
    ret[(i + 3:9 - 1) %% 10 + 1] <- n
    ret
  }
  vapply(1:10, f, numeric(10))
})



part1 <- function(d) {
  prod(play_deterministic(d[[1]], d[[2]]))
}


part2 <- function(d) {
  wins <- play_quantum(d[[1]], d[[2]])
  format(max(wins), digits = 20)
}


d <- parse("test-input-d21.txt")
stopifnot(part1(d) == 739785)
stopifnot(part2(d) == "444356092776315")

d <- parse("input-d21.txt")
part1(d)
part2(d)
