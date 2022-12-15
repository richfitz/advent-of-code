parse_input <- function(path) {
  pos <- lapply(
    strsplit(readLines(path), " -> ", fixed = TRUE),
    function(x) vapply(strsplit(x, ","), as.integer, integer(2)) + 1L)
  nr <- max(vapply(pos, function(p) max(p[2L, ]), numeric(1))) + 1L
  m <- matrix(TRUE, nr, 1001L)
  for (p in pos) {
    for (i in seq_len(ncol(p) - 1L)) {
      m[p[2L, i]:p[2L, i + 1L], p[1L, i]:p[1, i + 1L]] <- FALSE
    }
  }
  m
}

## A back-tracking solve, keep track of the path taken by the last
## grain of sand because we want to start from the second to last
## position taken. The actual algorithm here is a little obscured a
## few tricks to help R do this efficiently (especially using only a
## single vector to track the path and separately a 'length' value
## that lets us keep track of how much of this to look at).
##
## This works because if the path gets blocked, it's blocked because a
## grain of sand rests at the last part of a path, so we can just go
## right there and see if it falls. We can then wind all the way back
## to the start this way, with relatively few comparisons.
##
## There's a bit of a short-circuiting/assignment trick in the if
## statement of the for loop that is a bit gory but quite concice.
part1 <- function(m) {
  nr <- nrow(m)
  drop <- function(d) {
    x <- d[[1L]]
    pos <- x[[d[[2L]]]]
    for (i in (d[[2L]] + 1L):nr) {
      if (m[p <- pos + 1L] || m[p <- pos + 1L - nr] || m[p <- pos + 1L + nr]) {
        pos <- x[[i]] <- p
      } else {
        return(list(x, i - 2L, pos))
      }
    }
    NULL
  }
  x <- list(c(nr * 500L + 1L, rep_len(NA_integer_, nr - 1L)), 1L)
  for (i in seq_along(m)) {
    if (is.null(x <- drop(x))) {
      return(i - 1L)
    }
    m[[x[[3L]]]] <- FALSE
  }
}

## For the final part, we can fill in the final triangles directly,
## with a much simpler comparison - fill in row by row and look in the
## 3 above neighbours to see if there is a sand source.
part2 <- function(m) {
  sand <- seq_len(ncol(m)) == 501L
  tot <- 1L
  for (i in 2:nrow(m)) {
    j <- (500L - i):(501L + i)
    tot <- tot + sum(sand[j] <- (sand[j - 1] | sand[j] | sand[j + 1]) & m[i, j])
  }
  tot
}

m <- parse_input("test-input-d14.txt")
stopifnot(part1(m) == 24)
stopifnot(part2(m) == 93)

m <- parse_input("input-d14.txt")
part1(m)
part2(m)
