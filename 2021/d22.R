## Consider this in 2d where we can plot it easily and also get a
## naive version working.  The overlap rules are the same and the
## extension should come naturally.
parse <- function(path) {
  txt <- readLines(path)
  on <- grepl("^on ", txt)
  at <- matrix(unlist(strsplit(txt, "[^0-9-]+")), ncol = 7, byrow = TRUE)[, -1]
  storage.mode(at) <- "integer"
  lapply(seq_along(txt), function(i)
    list(on = on[i], at = matrix(at[i, ], 2)))
}


solve_naive <- function(d) {
  seq_range <- function(r) seq(r[[1]], r[[2]])
  m <- array(0, rep(101, 3))
  for (el in d) {
    at <- el$at + 51 # offset so that -50 -> 1
    m[seq_range(at[, 1]), seq_range(at[, 2]), seq_range(at[, 3])] <- el$on
  }
  sum(m)
}


intersect_line <- function(a, b) {
  right <- min(a[2], b[2])
  left <- max(a[1], b[1])
  if (right < left) {
    NULL
  } else {
    c(left, right)
  }
}


##  a  b main intersect
##  +  T    +         -
##  +  F    -         -
##  -  T    +         +
##  -  F    -         +
intersect_3d <- function(a, b) {
  miss <-
    is.null(x <- intersect_line(a$at[, 1], b$at[, 1])) ||
    is.null(y <- intersect_line(a$at[, 2], b$at[, 2])) ||
    is.null(z <- intersect_line(a$at[, 3], b$at[, 3]))
  if (miss) {
    return(NULL)
  }
  list(on = !a$on, at = cbind(x, y, z, deparse.level = FALSE))
}


solve_intersect <- function(d) {
  ans <- d[1]
  x <- d[[2]]
  for (x in d[-1]) {
    tmp <- lapply(ans, intersect_3d, x)
    i <- !vapply(tmp, is.null, TRUE)
    tmp <- tmp[!vapply(tmp, is.null, TRUE)]
    ans <- c(ans, if (x$on) list(x), tmp)
  }
  sum(vapply(ans, function(x)
    prod(diff(x$at) + 1) * (if (x$on) 1 else -1),
    numeric(1)))
}


part1 <- function(d) {
  i <- vapply(d, function(x) all(x$at >= -50 & x$at <= 50), TRUE)
  solve_naive(d[i])
}


part2 <- function(d) {
  i <- vapply(d, function(x) all(x$at >= -50 & x$at <= 50), TRUE)
  ## We then require that none of the large cubes intersect with the
  ## core.  This is true for my input and all examples, but may not be
  ## generally true.
  core <- solve_naive(d[i])
  outer <- solve_intersect(d[!i])
  format(core + outer, digits = 20)
}

d <- parse("test-input-d22-1.txt")
stopifnot(part1(d) == 39)

d <- parse("test-input-d22-2.txt")
stopifnot(part1(d) == 590784)

d <- parse("test-input-d22-3.txt")
stopifnot(part1(d) == 474140)
stopifnot(part2(d) == "2758514936282235")

d <- parse("input-d22.txt")
part1(d) # 598616
part2(d) # 1193043154475246
