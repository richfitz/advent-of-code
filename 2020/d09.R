valid <- function(d, n) {
  keep <- lower.tri(diag(n))
  k <- cbind(rep(seq_len(n), n), rep(seq_len(n), each = n))[keep, ]
  f <- function(i) {
    sort(d[i - k[, 1]] + d[i - k[, 2]])
  }
  t(vapply(seq(n + 1, length(d)), f, numeric(nrow(k))))
}

find_mismatch <- function(d, n) {
  v <- valid(d, n)
  x <- d[-seq_len(n)]
  x[!apply(x - v == 0, 1, any)]
}

find_sequence <- function(d, target) {
  for (n in seq_along(d)[-1]) {
    m <- outer(seq_len(length(d) - n + 1L), seq_len(n) - 1L, "+")
    tot <- rowSums(matrix(d[c(m)], nrow(m)))
    if (any(tot == target)) {
      return(sum(range(d[m[tot == target, ]])))
    }
  }
}

find_mismatch(as.numeric(readLines("test-input-d09.txt")), 5L)
find_sequence(as.numeric(readLines("test-input-d09.txt")), 127)

d <- as.numeric(readLines("input-d09.txt"))
part1 <- find_mismatch(d, 25L)
part2 <- sum(find_sequence(d, part1))
message(sprintf("Part 1: %s\nPart 2: %s", part1, part2))
