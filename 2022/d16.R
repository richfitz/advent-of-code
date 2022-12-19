parse_input <- function(path) {
  re <- "Valve (.+) has flow rate=(.+); tunnels? leads? to valves? (.*)"
  txt <- readLines(path)
  name <- sub(re, "\\1", txt)
  rate <- as.integer(sub(re, "\\2", txt))
  dest <- lapply(strsplit(sub(re, "\\3", txt), ", "), match, name)

  i <- rate > 0
  i <- which(i)[order(rate[i], decreasing = TRUE)]

  dist <- vapply(i, time, integer(length(name)), dest)
  dist <- dist[c(which(name == "AA"), i), ] + 1L
  dimnames(dist) <- list(c("AA", name[i]), name[i])

  list(dist = dist, rate = setNames(rate[i], name[i]))
}

## Naive bfs approach to compute distance matrix, does a bit too much
## work but is fairly easy at least.
time <- function(i, dest) {
  v <- rep(NA_integer_, length(dest))
  q <- dest[[i]]
  v[[i]] <- 0L
  for (j in seq_along(dest)) {
    q <- q[is.na(v[q])]
    if (length(q) == 0L) {
      break
    }
    v[q] <- j
    q <- unique(unlist(dest[q])) # easier with adjacency matrix
  }
  v
}

vmax <- function(a, b) {
  i <- b > a
  a[i] <- b[i]
  a
}

search <- function(d, part2 = FALSE) {
  best <- list2env(list(value = 0, n = 0), parent = emptyenv())
  descend <- function(at, wait, flow, remaining, idx) {
    x <- list(
      rate[idx] * (remaining - wait[[1]] - dist[at[[1]], idx, drop = FALSE]),
      rate[idx] * (remaining - wait[[2]] - dist[at[[2]], idx, drop = FALSE]))
    xm <- vmax(x[[1]], x[[2]])
    if (!all(k <- xm > 0)) {
      idx <- idx[k]
      x <- list(x[[1L]][k], x[[2L]][k])
      xm <- xm[k]
    }
    if (length(idx) == 0L) {
      if (flow > best$value) {
        best$value <- flow
      }
    } else if (flow + sum(xm) - 2 * length(xm) - 2 > best$value) {
      k <- if (wait[[1L]] == 0) 1L else 2L
      xk <- x[[k]]
      for (j in order(xk, decreasing = TRUE)) {
        if (xk[j] < 0) {
          break
        }
        at_j <- at
        at_j[[k]] <- idx[[j]] + 1L
        wait_j <- wait
        wait_j[[k]] <- dist[at[[k]], idx[[j]]]
        dt <- min(wait_j)
        descend(at_j, wait_j - dt, flow + xk[[j]], remaining - dt, idx[-j])
      }
    }
  }

  ## these are immutable now
  dist <- unname(d$dist)
  rate <- unname(d$rate)
  if (part2) {
    descend(c(1L, 1L), c(0L, 0L), 0L, 26L, seq_along(rate))
  } else {
    descend(c(1L, 1L), c(0L, 30L), 0L, 30L, seq_along(rate))
  }
  list(flow = best$value)
}

part1 <- function(d) {
  search(d, FALSE)$flow
}

part2 <- function(d) {
  search(d, TRUE)$flow
}

d <- parse_input("test-input-d16.txt")
stopifnot(part1(d) == 1651)
stopifnot(part2(d) == 1707)

d <- parse_input("input-d16.txt")
part1(d) # 1986
part2(d) # 2464
