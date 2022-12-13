parse <- function(txt) {
  matrix(as.integer(unlist(strsplit(txt, NULL))), 10, 10, TRUE)
}

state <- function(m) {
  dr <- c(-1, -1, -1, 0, 0, 1, 1, 1)
  dc <- c(-1, 0, 1, -1, 1, -1, 0, 1)
  nr <- outer(dr, c(row(m)), "+")
  nc <- outer(dc, c(col(m)), "+")
  ok <- nr > 0 & nr <= nrow(m) & nc > 0 & nc <= ncol(m)
  idx <- (nc - 1) * nrow(m) + nr
  near <- lapply(seq_along(m), function(i) idx[ok[, i], i])
  list(step = 0L, energy = m, near = near, total = 0L)
}

step <- function(state) {
  energy <- state$energy + 1L
  flash <- matrix(FALSE, nrow(energy), ncol(energy))

  i <- energy > 9
  if (any(i)) {
    while (any(i)) {
      flash[i] <- TRUE
      j <- unlist(state$near[i])
      energy <- energy + tabulate(j, 100)
      i <- energy > 9 & !flash
    }
    energy[flash] <- 0L
  }

  state$energy <- energy
  state$flash <- flash
  state$total <- state$total + sum(flash)
  state$step <- state$step + 1L

  state
}

run <- function(state, to) {
  for (i in seq_len(to - state$step)) {
    state <- step(state)
  }
  state
}

part1 <- function(m) {
  run(state(m), 100)$total
}

part2 <- function(m) {
  s <- state(m)
  repeat {
    s <- step(s)
    if (all(s$flash)) {
      return(s$step)
    }
  }
}

m <- parse(readLines("test-input-d11.txt"))
s <- state(m)

stopifnot(part1(m) == 1656)
stopifnot(part2(m) == 195)

m <- parse(readLines("input-d11.txt"))
part1(m)
part2(m)
