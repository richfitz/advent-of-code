parse <- function(path) {
  d <- strsplit(readLines(path), NULL)
  m <- matrix(as.integer(unlist(d)), length(d), byrow = TRUE)
  ## TODO: add boundary immediately
  m
}


add_boundary <- function(m) {
  m_pad <- array(9, dim(m) + 2)
  m_pad[seq_len(nrow(m)) + 1, seq_len(ncol(m)) + 1] <- m
  m_pad
}


find_basins <- function(m) {
  m_pad <- add_boundary(m)
  i <- seq_len(nrow(m))
  j <- seq_len(ncol(m))
  m_right <- m_pad[i + 1, j]
  m_left <- m_pad[i + 1, j + 2]
  m_down <- m_pad[i, j + 1]
  m_up <- m_pad[i + 2, j + 1]
  unname(which(m_right > m & m_left > m & m_down > m & m_up > m, TRUE))
}


part1 <- function(m) {
  m_pad <- add_boundary(m)
  i <- seq_len(nrow(m))
  j <- seq_len(ncol(m))
  m_right <- m_pad[i + 1, j]
  m_left <- m_pad[i + 1, j + 2]
  m_down <- m_pad[i, j + 1]
  m_up <- m_pad[i + 2, j + 1]
  sum(m[m_right > m & m_left > m & m_down > m & m_up > m] + 1)
}


flood_fill <- function(p, m, id, to) {
  while (nrow(p) > 0) {
    p <- p[m[p] < 9 & id[p] == 0, , drop = FALSE]
    if (nrow(p) > 0) {
      id[p] <- to
      r <- p[, 1]
      c <- p[, 2]
      p <- cbind(c(r + 1L, r - 1L, r,      r    ),
                 c(c,      c,      c + 1L, c - 1L))
    }
  }

  id
}


classify <- function(m, p) {
  p <- find_basins(m)
  m_pad <- add_boundary(m)
  id <- array(0, dim(m_pad))
  for (i in seq_len(nrow(p))) {
    id <- flood_fill(p[i, , drop = FALSE] + 1, m_pad, id, i)
  }
  id[id == 0] <- NA
  id
}


part2 <- function(m) {
  m_pad <- add_boundary(m)
  p <- find_basins(m)
  id <- classify(m, p)
  n <- tabulate(id)
  prod(head(sort(n, decreasing = TRUE), 3))
}

m <- parse("test-input-d09.txt")
stopifnot(part1(m) == 15)
stopifnot(part2(m) == 1134)

m <- parse("input-d09.txt")
part1(m)
part2(m)
