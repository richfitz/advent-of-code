parse_input <- function(path) {
  d <- strsplit(readLines(path), NULL)
  m <- matrix(match(unlist(d), c("S", letters, "E")), length(d), byrow = TRUE)
  list(m = m, d = search(m, which(m == 28)))
}

near <- function(m, idx, r, c, nr, nc) {
  v <- c(if (r > 1) m[r - 1, c] else -Inf, if (r < nr) m[r + 1, c] else -Inf,
         if (c > 1) m[r, c - 1] else -Inf, if (c < nc) m[r, c + 1] else -Inf)
  c(idx - 1, idx + 1, idx - nr, idx + nr)[v - m[r, c] >= -1]
}

search <- function(m, target) {
  m[m == 1] <- 2
  dist <- rep(Inf, length(m))
  dist[target] <- 0L
  pending <- rep_len(TRUE, length(m))
  mr <- row(m)
  mc <- col(m)
  while (any(pending)) {
    u <- which(pending)[which.min(dist[pending])]
    pending[u] <- FALSE
    v <- near(m, u, mr[u], mc[u], nrow(m), ncol(m))
    i <- pending[v]
    dist[v[i]] <- pmin(dist[v[i]], 1 + dist[[u]])
  }
  dist
}

part1 <- function(d) {
  d$d[d$m == 1]
}

part2 <- function(d) {
  min(d$d[d$m <= 2])
}

test <- parse_input("test-input-d12.txt")
stopifnot(part1(test) == 31)
stopifnot(part2(test) == 29)

d <- parse_input("input-d12.txt")
part1(d)
part2(d)
