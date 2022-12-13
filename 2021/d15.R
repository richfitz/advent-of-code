parse <- function(path) {
  d <- strsplit(readLines(path), NULL)
  matrix(as.integer(unlist(d)), length(d), byrow = TRUE)
}


find_near <- function(m) {
  dr <- c(1, 0, -1, 0)
  dc <- c(0, 1, 0, -1)
  nr <- outer(dr, c(row(m)), "+")
  nc <- outer(dc, c(col(m)), "+")
  ok <- nr > 0 & nr <= nrow(m) & nc > 0 & nc <= ncol(m)
  idx <- (nc - 1) * nrow(m) + nr
  ret <- lapply(seq_along(m), function(i) idx[ok[, i], i])
  dim(ret) <- c(nrow(m), ncol(m))
  ret
}


index <- function(k, n) {
  i <- if (k <= n) seq_len(k) else (k - n + 1):n
  cbind(rev(i), i, deparse.level = 0)
}


fix_backtrack <- function(check, d, m, near) {
  check <- check[, 1] + nrow(d) * (check[, 2] - 1)
  while (length(check) > 0) {
    idx <- which.min(d[check])
    i <- check[idx]
    i_near <- near[[i]]
    d_near <- d[i_near]
    d_new <- min(d_near + m[[i]])
    if (d_new < d[[i]]) {
      d[[i]] <- d_new
      check <- c(check[-idx], i_near[is.finite(d_near)])
    } else {
      check <- check[-idx]
    }
  }
  d
}


solve <- function(m) {
  near <- find_near(m)
  n <- nrow(m)
  d <- matrix(Inf, n, n)
  d[[1]] <- 0L

  for (k in 2:(2 * n - 1)) {
    i <- index(k, n)
    j <- index(k - 1, n)

    prev <- d[j]
    prev_min <- pmin(prev[-1], prev[-length(prev)])
    if (k <= n) {
      prev_min <- c(prev[[1]], prev_min, prev[[length(prev)]])
    }
    curr <- d[i] <- m[i] + prev_min

    ## Look to see if we can backtrack
    curr_min <- pmin(curr[-1], curr[-length(curr)])
    if (k > n) {
      curr_min <- c(curr[[1]], curr_min, curr[[length(curr)]])
    }
    check <- d[j] > m[j] + curr_min
    if (any(check)) {
      d <- fix_backtrack(j[check, , drop = FALSE], d, m, near)
    }
  }

  d
}


expand <- function(m) {
  nr <- nrow(m)
  nc <- ncol(m)
  res <- matrix(NA, nr * 5, nc * 5)
  for (i in 1:5) {
    for (j in 1:5) {
      ii <- seq((i - 1) * nr + 1L, length.out = nr)
      jj <- seq((j - 1) * nc + 1L, length.out = nc)
      res[ii, jj] <- (m + i + j - 2)
    }
  }

  res[res > 9] <- res[res > 9] - 9
  res
}


part1 <- function(m) {
  solve(m)[[length(m)]]
}


part2 <- function(m) {
  m <- expand(m)
  solve(m)[[length(m)]]
}


m <- parse("test-input-d15.txt")
part1(m)
part2(m)

m <- parse("input-d15.txt")
part1(m)
part2(m)
