shapes <- list(15L, c(2L, 7L, 2L), c(7L, 4L, 4L), c(1L, 1L, 1L, 1L), c(3L, 3L))
start <- list(
  c(1L, 1L, 1L, 2L, 1L, 4L, 4L,  4L, 2L, 4L, 4L, 8L,  4L,  8L,  8L,  8L), # -
  c(1L, 1L, 1L, 4L, 1L, 4L, 4L,  8L, 2L, 4L, 4L, 16L, 4L, 16L, 16L, 16L), # +
  c(1L, 1L, 1L, 4L, 1L, 4L, 4L,  8L, 2L, 4L, 4L, 16L, 4L, 16L, 16L, 16L), # j
  c(1L, 1L, 1L, 4L, 1L, 4L, 4L, 16L, 2L, 4L, 4L, 16L, 4L, 16L, 16L, 64L), # I
  c(1L, 1L, 1L, 4L, 1L, 4L, 4L, 16L, 2L, 4L, 4L, 16L, 4L, 16L, 16L, 32L)) # o

parse_input <- function(path) {
  strsplit(readLines(path), NULL)[[1]] == ">"
}

## Turn a vector 'x' into a circular list. This is more of a faff than
## ideal but allows us to pull out whole vectors of circularised
## values, which simplifies the initial drop nicely later.
circular <- function(x) {
  i <- 0L
  function(n = 1L) {
    if (n == 1) {
      x[[i <<- if (i == length(x)) 1L else i + 1L]]
    } else {
      j <- (i:(i + n - 1L) %% length(x)) + 1L
      i <<- j[[n]]
      x[j]
    }
  }
}

simulate <- function(move, n_rocks) {
  history <- c(127L, integer(ceiling(n_rocks / 5 * 13)))
  height <- 1L
  height_change <- integer(n_rocks)
  for (idx in seq_len(n_rocks)) {
    i <- (idx - 1L) %% length(shapes) + 1L
    x <- start[[i]][[sum(move(4) * c(1, 2, 4, 8)) + 1L]] * shapes[[i]]
    hx <- length(x)
    for (j in seq_len(height)) {
      xc <- if (j >= hx) x else x[seq_len(j)]
      ih <- seq_along(xc) + height - j
      if (any(bitwAnd(history[ih], xc) > 0)) {
        i <- (height - j + 2L):(height - j + hx + 1L)
        history[i] <- history[i] + x
        height_change[[idx]] <- max(hx - j + 1L, 0L)
        height <- height + height_change[[idx]]
        break
      } else {
        if (move()) {
          xs <- if (any(bitwAnd(x, 64L)) > 0) x else x * 2L
        } else {
          xs <- if (any(bitwAnd(x, 1L) > 0)) x else x %/% 2L
        }
        x <- if (all(bitwAnd(history[ih], xs[seq_along(xc)]) == 0L)) xs else x
      }
    }
  }
  height_change
}

## Previously I had a nice general cycle finding bit of code here, but
## it's just too much really. It did find the shortest repeating
## length (whereas this finds the shortest longer than 100)
project <- function(x, n_rocks) {
  m <- gregexpr(intToUtf8(tail(x, 100) + 48), intToUtf8(x + 48))[[1]]
  start <- m[[1]]
  len <- m[[2]] - m[[1]]
  p <- x[(start + 1):(start + len)] # the pattern
  sum(x[seq_len(start)]) + sum(p[seq_len((n_rocks - start) %% len)]) +
    (n_rocks - start) %/% len * sum(p)
}

part1 <- function(x) {
  format(project(x, 2022), digits = 20)
}

part2 <- function(x) {
  format(project(x, 1000000000000), digits = 20)
}

d <- parse_input("test-input-d17.txt")
res <- simulate(circular(d), 2022)
stopifnot(sum(res) == 3068)
stopifnot(part1(res) == "3068")
stopifnot(part2(res) == "1514285714288")

d <- parse_input("input-d17.txt")
res <- simulate(circular(d), 5000) # 16 + 1856 * 2 would be enough :P
part1(res) # 3102
part2(res) # 1539823008825
